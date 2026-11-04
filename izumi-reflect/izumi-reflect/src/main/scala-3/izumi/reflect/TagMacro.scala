package izumi.reflect

import scala.quoted.{Expr, Quotes, Type, Varargs}
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef, RuntimeAPI}
import izumi.reflect.dottyreflection.{Inspect, InspectorBase, ReflectionUtil, TypeInspections}
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, AppliedReference, Boundaries, FullReference, IntersectionReference, NameReference, RefinementDecl, SymName, TypeParam, UnionReference, Variance, WildcardReference}

import scala.collection.immutable.Queue
import scala.collection.mutable

object TagMacro {
  def createTagExpr[A <: AnyKind: Type](using Quotes): Expr[Tag[A]] =
    new TagMacro().createTagExpr[A]
}

final class TagMacro(using override val qctx: Quotes) extends InspectorBase {
  import qctx.reflect._

  override def shift: Int = 0

  private val tagSymbol = Symbol.requiredClass("izumi.reflect.Tag")
  private val tagSymbolTypeRef = tagSymbol.typeRef

  private val tagObjSymbol = Symbol.requiredModule("izumi.reflect.Tag")
  private val tagObjApplyMethodSym = tagObjSymbol.declaredMethod("apply").find(_.paramSymss(1).size == 2).get

  private lazy val tagWildCardTpe = Type.of[Tag[?]]

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val owners = getClassDefOwners(Symbol.spliceOwner)
    val typeRepr = TypeRepr.of[A]._dealiasSimplifiedFull
    if (allPartsStrong(owners, typeRepr)) {
      createTag[A](typeRepr)
    } else {
      summonCombinedTag[A](owners, typeRepr)
    }
  }

  private def createTag[A <: AnyKind](typeRepr0: TypeRepr)(using Type[A]): Expr[Tag[A]] = {
    val typeRepr = typeRepr0._etaExpandTypeRef // convert HKT type refs to type lambdas manually as an optimization. This required an additional splice level before.
    val ltt = Inspect.inspectTypeRepr(typeRepr)
    val cls = closestClassOfTypeRepr(typeRepr)
    Apply(
      fun = TypeApply(
        fun = Select(qualifier = Ref.term(tagObjSymbol.termRef), symbol = tagObjApplyMethodSym),
        args = List(TypeTree.of[A])
      ),
      args = List(cls.asTerm, ltt.asTerm)
    ).asExpr.asInstanceOf[Expr[Tag[A]]]
  }

  private def summonCombinedTag[T <: AnyKind: Type](owners: Set[Symbol], typeReprDealiased: TypeRepr): Expr[Tag[T]] = {

    def summonLTT(typeRepr: TypeRepr): Expr[LightTypeTag] = {
      typeRepr match {
        case TypeBounds(low, high) =>
          val lowTag = summonTag[T](low)
          val highTag = summonTag[T](high)
          '{ LightTypeTag.wildcardType($lowTag.tag, $highTag.tag) }
        case _ =>
          val result = summonTag[T](typeRepr)
          '{ $result.tag }
      }
    }

    def summonIfNotLambdaParamOf(typeRepr: TypeRepr, lam: TypeRepr): Expr[Option[LightTypeTag]] = {
      if (isLambdaParamOf(typeRepr, lam)) {
        '{ None }
      } else {
        val tag = summonLTT(typeRepr)
        '{ Some($tag) }
      }
    }

    def isLambdaParamOf(typeRepr: TypeRepr, lam: TypeRepr): Boolean = {
      typeRepr match {
        case ref: ParamRef if ref.binder == lam => true
        case _ => false
      }
    }

    typeReprDealiased match {
      case outerLambda: TypeLambda =>
        outerLambda.resType match {
          case AppliedType(ctorTpe, typeArgsTpes) =>
            val paramsRange = 0 until outerLambda.paramNames.size
            val isSimpleApplication = typeArgsTpes.collect {
              case ref: ParamRef if ref.binder == outerLambda => ref.paramNum
            } == paramsRange

            val constructorTag = summonTag[T](ctorTpe)
            if (isSimpleApplication) {
              val argsTags = Expr.ofList(typeArgsTpes.map(a => summonIfNotLambdaParamOf(a, outerLambda)))
              '{ Tag.appliedTagNonPos[T](${ constructorTag }, ${ argsTags }) }
            } else {
              val distinctNonParamArgsTypes = typeArgsTpes.filter(!isLambdaParamOf(_, outerLambda)).distinct
              val outerLambdaParamArgsTypeParamRefs = paramsRange.map(outerLambda.param(_)).toList

              val arity = 1 + distinctNonParamArgsTypes.size + outerLambdaParamArgsTypeParamRefs.size
              val fullParamTail = (distinctNonParamArgsTypes ++ outerLambdaParamArgsTypeParamRefs)
                .iterator.distinct.zipWithIndex
              val typeArgToLambdaParameterMap = fullParamTail.map {
                case (argTpe, idx) =>
                  val idxPlusOne = idx + 1
                  val lambdaParameter = SymName.LambdaParamName(idxPlusOne, LightTypeTagRef.LambdaConstants.tagMacro, arity)
                  argTpe -> lambdaParameter
              }.toMap

              val usageOrderDistinctNonLambdaArgs = distinctNonParamArgsTypes.map(t => typeArgToLambdaParameterMap(t))
              val declarationOrderLambdaParamArgs = outerLambdaParamArgsTypeParamRefs.map(t => typeArgToLambdaParameterMap(t))
              val completeTail = usageOrderDistinctNonLambdaArgs ::: declarationOrderLambdaParamArgs

              val usages = typeArgsTpes.map(t => TypeParam(NameReference(typeArgToLambdaParameterMap(t)), Variance.Invariant))

              // we give a distinct lambda parameter to the constructor, even if constructor is one of the type parameters
              val firstParamIdx = 0
              assert(completeTail.size + 1 == arity)
              val ctorLambdaParameter = SymName.LambdaParamName(firstParamIdx, LightTypeTagRef.LambdaConstants.tagMacro, arity)

              val ctorApplyingLambda =
                LightTypeTagRef.Lambda(
                  ctorLambdaParameter :: completeTail,
                  FullReference(ctorLambdaParameter, usages)
                )

              log(s"""HK non-trivial lambda construction:
                     |ctorApplyingLambda=$ctorApplyingLambda
                     |usageOrderNonLambdaArgs=$usageOrderDistinctNonLambdaArgs
                     |declarationOrderLambdaParamArgs=$declarationOrderLambdaParamArgs
                     |""".stripMargin)

              val argTagsExceptCtor = {
                val nonParamArgsDealiased = distinctNonParamArgsTypes.map(_._dealiasSimplifiedFull)
                log(s"HK COMPLEX Now summoning tags for args=$nonParamArgsDealiased outerLambdaParams=$outerLambdaParamArgsTypeParamRefs")
                Expr.ofList(
                  nonParamArgsDealiased.map(t => '{ Some(${ summonLTT(t) }) }) ++ outerLambdaParamArgsTypeParamRefs.map(_ => '{ None })
                )
              }

              val outerLambdaReprTag = Inspect.makeParsedLightTypeTagImpl(LightTypeTag(ctorApplyingLambda, Map.empty, Map.empty))
              '{
                val ctorTag = ${ constructorTag }.asInstanceOf[Tag[Any]]
                Tag.appliedTagNonPosAux[T](ctorTag.closestClass, ${ outerLambdaReprTag }, Some(ctorTag.tag) :: ${ argTagsExceptCtor })
              }
            }

          case other =>
            // TODO add support for and/or/refinement, see test with `IntersectionBlockingIO`
            report.warning(
              s"""TODO: Pathological intersection refinement result in lambda being reconstructed result=`${other.show}` in the rhs of type lambda lam=`${outerLambda.show}`
                 |Only simple applied types of form F[A] are supported in results of type lambdas. The generated tag will not work correctly.""".stripMargin
            )
            createTag[T](outerLambda)
        }

      case AppliedType(ctor, args) =>
        val ctorTag = summonTag[T](ctor)
        val argsTags = Expr.ofList(args.map(summonLTT))
        '{ Tag.appliedTag[T](${ ctorTag }, ${ argsTags }) }

      case andType: AndType =>
        val tpes = flattenAnd(andType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTT))
        val cls = Literal(ClassOfConstant(lubClassOf(typeReprDealiased, tpes))).asExpr.asInstanceOf[Expr[Class[?]]]
        val dummyAnyStructLtt = {
          // FIXME add constructor for intersections without the unused on Scala 3 struct type
          Inspect.inspectAny[Any]
        }
        '{ Tag.refinedTag[T](${ cls }, ${ ltts }, ${ dummyAnyStructLtt }, Map.empty) }

      case orType: OrType =>
        val tpes = flattenOr(orType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTT))
        val cls = Literal(ClassOfConstant(lubClassOf(typeReprDealiased, tpes))).asExpr.asInstanceOf[Expr[Class[?]]]
        '{ Tag.unionTag[T](${ cls }, ${ ltts }) }

      case refinement: Refinement =>
        val (members, parent) = flattenRefinements(refinement)
        val cls = closestClassOfTypeRepr(parent)
        val parentLtt = summonLTT(parent)

        val (allTypeMembers, termMembers) = members.partitionMap {
          case (s, n, tb: TypeBounds) if allPartsStrong(owners, tb) => Left(Left((n, tb)))
          case (_, n, TypeBounds(lo, hi)) if lo == hi => Left(Right((n, hi)))
          case (_, _, tb @ TypeBounds(lo, hi)) =>
            report.errorAndAbort(
              s"TagMacro: resolving type parameters inside type bounds is not supported, got weak types in bounds=${tb.show}, in type=$typeReprDealiased"
            )
          case x => Right(x)
        }
        val (strongTypeBounds, weakTypeMembers) = allTypeMembers.partitionMap(identity)
        // FIXME: once we add resolution for method/val members too, not just type members
        //  this struct will no longer be 'weak'. In fact we'll want to add a new constructor
        //  instead of `refinedTag` that will be better suited to fully resolved struct tags
        val termAndStrongTpesOnlyWeakStructLtt = {
          val termOnlyRefinementTypeRepr = termMembers.foldRight(defn.AnyRefClass.typeRef: TypeRepr) {
            case ((_, name, tpe), refinement) =>
              Refinement(parent = refinement, name = name, info = tpe)
          }
          val withStrongTpesRefinementTypeRepr = strongTypeBounds.foldRight(termOnlyRefinementTypeRepr) {
            case ((name, tpe), refinement) =>
              Refinement(parent = refinement, name = name, info = tpe)
          }
          val weakTermTypes = collectWeakTermTypes(owners, termMembers)
          if (weakTermTypes.isEmpty) {
            Inspect.inspectTypeRepr(withStrongTpesRefinementTypeRepr)
          } else {
            inspectTermRefinementWithWeakTypes(withStrongTpesRefinementTypeRepr, weakTermTypes, summonLTT)
          }
        }
        val resolvedTypeMemberLtts = weakTypeMembers.map {
          case (name, tpe) => '{ (${ Expr(name) }, ${ summonLTT(tpe) }) }
        }
        // NB: we're resolving LTTs anew for all type members here, instead of optimizing
        // to resolve only for 'weak' members as in Scala 2.
        log(
          s"""Got refinement $refinement
             |parent=$parent
             |members=$members
             |closestClass=$cls
             |""".stripMargin
        )
        '{ Tag.refinedTag[T](${ cls }, List(${ parentLtt }), ${ termAndStrongTpesOnlyWeakStructLtt }, Map(${ Varargs(resolvedTypeMemberLtts) }: _*)) }

      // error: the entire type is just a proper type parameter with no type arguments
      // it cannot be resolved further
      case x if ReflectionUtil.topLevelWeakType(owners, Set.empty, x) =>
        val tStr = x.show
        val implicitMessage = defaultImplicitError.replace("${T}", tStr)
        report.errorAndAbort(s"""$tStr is a type parameter without an implicit Tag!
                                |  $implicitMessage
                                |""".stripMargin)

      case _ =>
        report.errorAndAbort(s"Unsupported type in TagMacro.summonCombinedTag: $typeReprDealiased")
    }
  }

  private def closestClassOfTypeRepr(typeRepr: TypeRepr): Expr[Class[?]] = {
    Literal(ClassOfConstant(lubClassOf(typeRepr, intersectionUnionRefinementClassPartsOf(typeRepr)))).asExpr.asInstanceOf[Expr[Class[?]]]
  }

  private def collectWeakTermTypes(owners: Set[Symbol], termMembers: Queue[(Symbol, String, TypeRepr)]): List[TypeRepr] = {
    val seen = mutable.LinkedHashMap.empty[String, TypeRepr]
    def go(tpe0: TypeRepr, localLambdas: Set[TypeRepr]): Unit = {
      tpe0._dealiasSimplifiedFull match {
        case x if ReflectionUtil.topLevelWeakType(owners, localLambdas, x) =>
          val key = x.typeSymbol.fullName
          if (!seen.contains(key)) {
            seen.put(key, x)
          }
        case poly @ PolyType(_, bounds, res) =>
          val nextLambdas = localLambdas + poly
          bounds.foreach(go(_, nextLambdas))
          go(res, nextLambdas)
        case tl @ TypeLambda(_, bounds, body) =>
          val nextLambdas = localLambdas + tl
          bounds.foreach(go(_, nextLambdas))
          go(body, nextLambdas)
        case MethodType(_, args, res) =>
          args.foreach(go(_, localLambdas))
          go(res, localLambdas)
        case AppliedType(tycon, args) =>
          go(tycon, localLambdas)
          args.foreach(go(_, localLambdas))
        case AndType(lhs, rhs) =>
          go(lhs, localLambdas)
          go(rhs, localLambdas)
        case OrType(lhs, rhs) =>
          go(lhs, localLambdas)
          go(rhs, localLambdas)
        case TypeBounds(lo, hi) =>
          go(lo, localLambdas)
          go(hi, localLambdas)
        case ByNameType(underlying) =>
          go(underlying, localLambdas)
        case Refinement(parent, _, info) =>
          go(parent, localLambdas)
          go(info, localLambdas)
        case TypeRef(prefix, _) =>
          go(prefix, localLambdas)
        case TermRef(prefix, _) =>
          go(prefix, localLambdas)
        case ThisType(tref) =>
          go(tref, localLambdas)
        case _ =>
      }
    }
    termMembers.foreach {
      case (_, _, info) =>
        go(info, Set.empty)
    }
    seen.values.toList
  }

  private def inspectTermRefinementWithWeakTypes(
    refinementTpe: TypeRepr,
    weakTypes: List[TypeRepr],
    summonLTT: TypeRepr => Expr[LightTypeTag]
  ): Expr[LightTypeTag] = {
    val totalParams = weakTypes.size
    val symToLambdaParam: Map[SymName, SymName.LambdaParamName] = weakTypes.zipWithIndex.map {
      case (w, idx) =>
        val symName: SymName = w match {
          case p: ParamRef =>
            val lt = p.binder.asInstanceOf[LambdaType]
            SymName.LambdaParamName(p.paramNum, -1, lt.paramNames.size)
          case _ =>
            SymName.SymTypeName(w.typeSymbol.fullName)
        }
        val paramName = SymName.LambdaParamName(idx, LightTypeTagRef.LambdaConstants.tagMacro, totalParams)
        symName -> paramName
    }.toMap
    val lambdaParams = weakTypes.indices.map {
      idx =>
        SymName.LambdaParamName(idx, LightTypeTagRef.LambdaConstants.tagMacro, totalParams)
    }.toList

    def replaceApplied(ref: AppliedReference): AppliedReference = ref match {
      case IntersectionReference(refs) =>
        LightTypeTagRef.maybeIntersection(refs.map(replaceApplied))
      case UnionReference(refs) =>
        LightTypeTagRef.maybeUnion(refs.map(replaceApplied))
      case WildcardReference(boundaries) =>
        WildcardReference(replaceBoundaries(boundaries))
      case LightTypeTagRef.Refinement(reference, decls) =>
        val newDecls = decls.map {
          case RefinementDecl.Signature(name, input, output) =>
            RefinementDecl.Signature(name, input.map(replaceApplied), replaceApplied(output)): RefinementDecl
          case RefinementDecl.TypeMember(name, tpeRef) =>
            RefinementDecl.TypeMember(name, replaceAbstract(tpeRef)): RefinementDecl
        }
        LightTypeTagRef.Refinement(replaceApplied(reference), newDecls)
      case nr: AppliedNamedReference =>
        replaceNamed(nr)
    }

    def replaceNamed(ref: AppliedNamedReference): AppliedNamedReference = ref match {
      case NameReference(sym, boundaries, prefix) =>
        symToLambdaParam.get(sym) match {
          case Some(param) => NameReference(param, replaceBoundaries(boundaries), None)
          case None => NameReference(sym, replaceBoundaries(boundaries), prefix.map(replaceApplied))
        }
      case FullReference(sym, parameters, prefix) =>
        val newParams = parameters.map(p => TypeParam(replaceAbstract(p.ref), p.variance))
        symToLambdaParam.get(sym) match {
          case Some(param) => FullReference(param, newParams, None)
          case None => FullReference(sym, newParams, prefix.map(replaceApplied))
        }
    }

    def replaceBoundaries(b: Boundaries): Boundaries = b match {
      case Boundaries.Defined(bottom, top) => Boundaries.Defined(replaceAbstract(bottom), replaceAbstract(top))
      case Boundaries.Empty => Boundaries.Empty
    }

    def replaceAbstract(ref: AbstractReference): AbstractReference = ref match {
      case LightTypeTagRef.Lambda(input, output) =>
        LightTypeTagRef.Lambda(input, replaceAbstract(output))
      case a: AppliedReference =>
        replaceApplied(a)
    }

    val ref0 = TypeInspections.apply(using qctx)(refinementTpe)
    val fullDb0 = TypeInspections.fullDb(using qctx)(refinementTpe)
    val nameDb0 = TypeInspections.unappliedDb(using qctx)(refinementTpe)

    val rewrittenBody = replaceAbstract(ref0)
    val lambdaRef = LightTypeTagRef.Lambda(lambdaParams, rewrittenBody)
    val lambdaParamSet: Set[SymName] = lambdaParams.toSet
    def hasLambdaParam(r: AbstractReference): Boolean = {
      RuntimeAPI.unpack(r).exists(n => lambdaParamSet.contains(n.ref))
    }
    def maybeWrapLambda(r: AbstractReference): AbstractReference = r match {
      case l: LightTypeTagRef.Lambda => l
      case applied: AppliedReference =>
        if (hasLambdaParam(applied)) LightTypeTagRef.Lambda(lambdaParams, applied) else applied
    }
    val rewrittenFullDb = fullDb0.foldLeft(Map.empty[AbstractReference, Set[AbstractReference]]) {
      case (acc, (k: AppliedNamedReference, _)) if symToLambdaParam.contains(k.symName) =>
        acc
      case (acc, (k, vs)) =>
        val k1 = maybeWrapLambda(replaceAbstract(k))
        val vs1 = vs.map(v => maybeWrapLambda(replaceAbstract(v))).filterNot(_ == k1)
        if (vs1.nonEmpty) {
          acc.updated(k1, acc.getOrElse(k1, Set.empty) ++ vs1)
        } else {
          acc
        }
    }
    val cleanNameDb = nameDb0.filterNot {
      case (k, _) => symToLambdaParam.contains(k.ref)
    }
    val ctorLtt = LightTypeTag(lambdaRef, rewrittenFullDb, cleanNameDb)
    val ctorLttExpr = Inspect.makeParsedLightTypeTagImpl(ctorLtt)
    val argExprs = Expr.ofSeq(weakTypes.map(summonLTT))
    '{ $ctorLttExpr.combine($argExprs*) }
  }

  private def lubClassOf(specificTpe: TypeRepr, tpes: List[TypeRepr]): TypeRepr = {
    tpes.map(_.baseClasses) match {
      case h :: t =>
        val bases = h.to(mutable.LinkedHashSet)
        t.foreach {
          b =>
            val bBases = b.to(mutable.HashSet)
            bases.filterInPlace(bBases)
        }
        // rely on the fact that .baseClasses returns classes in order from most specific to least, therefore most specific class should be first.
        val baseClass = bases.headOption.getOrElse(defn.AnyClass)
        // try to recover type parameters of the specific type e.g. to support Arrays
        // see https://github.com/zio/izumi-reflect/issues/474 & https://github.com/scala/scala3/issues/21916
        specificTpe.baseType(baseClass)
      // FIXME: below doesn't work, need to treat AnyVals specially
//        bases.find(!_.typeRef.baseClasses.contains(defn.AnyValClass)).getOrElse(defn.AnyClass).typeRef
      case Nil =>
        defn.AnyClass.typeRef
    }
  }

  private def summonTag[T <: AnyKind](typeRepr: TypeRepr)(using outerCombinedType: Type[T]): Expr[Tag[?]] = {
    val tagTypeRepr = AppliedType(tagSymbolTypeRef, List(typeRepr))
    Implicits.search(tagTypeRepr) match {
      case s: ImplicitSearchSuccess =>
        s.tree.asExpr.asInstanceOf[Expr[Tag[?]]]
      case f: ImplicitSearchFailure =>
        val aStr = typeRepr.show
        val implicitMessage = defaultImplicitError.replace("${T}", aStr)
        val message = s"""Error when creating a combined tag for ${Type.show[T]}, when summoning Tag for part of that type $aStr:
                         |  $implicitMessage
                         |Structure of overall type was: `${TypeRepr.of[T]}`
                         |Structure of part of the type was: `$typeRepr
                         |Stack trace: ${locally {
                          import java.io.{PrintWriter, StringWriter}
                          val t = new Exception()
                          val sw = new StringWriter()
                          t.printStackTrace(new PrintWriter(sw))
                          sw.toString
                        }}""".stripMargin
        report.errorAndAbort(message)
    }
  }

  private inline val defaultImplicitError =
    "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
}
