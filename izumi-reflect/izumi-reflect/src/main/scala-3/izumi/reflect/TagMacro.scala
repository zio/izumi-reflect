package izumi.reflect

import scala.quoted.{Expr, Quotes, Type, Varargs}
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef}
import izumi.reflect.dottyreflection.{Inspect, InspectorBase, ReflectionUtil}
import izumi.reflect.macrortti.LightTypeTagRef.{FullReference, NameReference, SymName, TypeParam, Variance}

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

  private def createTag[A <: AnyKind](typeRepr0: TypeRepr): Expr[Tag[A]] = {
    val typeRepr = typeRepr0._etaExpandTypeRef // convert HKT type refs to type lambdas manually as an optimization. This required an additional splice level before.
    val ltt = Inspect.inspectTypeRepr(typeRepr)
    val cls = closestClassOfTypeRepr(typeRepr)
    Apply(
      fun = TypeApply(
        fun = Select(qualifier = Ref.term(tagObjSymbol.termRef), symbol = tagObjApplyMethodSym),
        args = List(Inferred(typeRepr))
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

    def summonLTTIfAvailable(typeRepr: TypeRepr): Option[Expr[LightTypeTag]] = {
      typeRepr match {
        case _: TypeBounds =>
          None
        case _ =>
          val tagTypeRepr = AppliedType(tagSymbolTypeRef, List(typeRepr))
          Implicits.search(tagTypeRepr) match {
            case s: ImplicitSearchSuccess =>
              val tagExpr = s.tree.asExpr.asInstanceOf[Expr[Tag[?]]]
              Some('{ $tagExpr.tag })
            case _: ImplicitSearchFailure =>
              None
          }
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

        members.foreach {
          case (_, _, tb @ TypeBounds(lo, hi)) if lo != hi && !allPartsStrong(owners, tb) =>
            report.errorAndAbort(
              s"TagMacro: resolving type parameters inside type bounds is not supported, got weak types in bounds=${tb.show}, in type=$typeReprDealiased"
            )
          case _ =>
        }

        val weakTypeMemberSubstitutions = {
          val resolvedWeakMembers = mutable.LinkedHashMap.empty[String, Expr[LightTypeTag]]
          def tryResolveWeak(part: TypeRepr, localBinders: Set[TypeRepr]): Unit = {
            if (ReflectionUtil.topLevelWeakType(owners, localBinders, part)) {
              val sym = part.typeSymbol
              if (!sym.isNoSymbol) {
                val key = sym.fullName
                if (!resolvedWeakMembers.contains(key)) {
                  summonLTTIfAvailable(part).foreach { ltt =>
                    resolvedWeakMembers += key -> ltt
                  }
                }
              }
            }
          }

          def collectWeakMembers(info0: TypeRepr, localBinders: Set[TypeRepr]): Unit = {
            val info = info0._dealiasSimplifiedFull
            tryResolveWeak(info, localBinders)
            info match {
              case AppliedType(tycon, args) =>
                collectWeakMembers(tycon, localBinders)
                args.foreach(collectWeakMembers(_, localBinders))
              case AndType(lhs, rhs) =>
                collectWeakMembers(lhs, localBinders)
                collectWeakMembers(rhs, localBinders)
              case OrType(lhs, rhs) =>
                collectWeakMembers(lhs, localBinders)
                collectWeakMembers(rhs, localBinders)
              case TypeRef(prefix, _) =>
                collectWeakMembers(prefix, localBinders)
              case TermRef(prefix, _) =>
                collectWeakMembers(prefix, localBinders)
              case ThisType(prefix) =>
                collectWeakMembers(prefix, localBinders)
              case TypeBounds(lo, hi) =>
                collectWeakMembers(lo, localBinders)
                collectWeakMembers(hi, localBinders)
              case lam @ TypeLambda(_, _, res) =>
                collectWeakMembers(res, localBinders + lam)
              case poly @ PolyType(_, bounds, res) =>
                bounds.foreach { case TypeBounds(lo, hi) =>
                  collectWeakMembers(lo, localBinders + poly)
                  collectWeakMembers(hi, localBinders + poly)
                }
                collectWeakMembers(res, localBinders + poly)
              case MethodType(_, argTypes, res) =>
                argTypes.foreach(collectWeakMembers(_, localBinders))
                collectWeakMembers(res, localBinders)
              case ByNameType(inner) =>
                collectWeakMembers(inner, localBinders)
              case refinementInfo: Refinement =>
                collectWeakMembers(refinementInfo.parent, localBinders)
                collectWeakMembers(refinementInfo.info, localBinders)
              case _: ParamRef | _: ConstantType | _: NoPrefix =>
              case _: TypeRepr =>
            }
          }

          members.iterator
            .flatMap { case (_, _, info) => refinementInfoToParts(info) }
            .foreach(collectWeakMembers(_, Set.empty))
          resolvedWeakMembers.toList
        }

        val unresolvedWeakStructLtt = Inspect.inspectTypeRepr(refinement)
        val resolvedWeakTypeMembersExpr = weakTypeMemberSubstitutions.map {
          case (name, ltt) => '{ (${ Expr(name) }, $ltt) }
        }
        val resolvedStructLtt = if (resolvedWeakTypeMembersExpr.isEmpty) {
          unresolvedWeakStructLtt
        } else {
          '{ Tag.resolveWeakTypeMembers(${ unresolvedWeakStructLtt }, Map(${ Varargs(resolvedWeakTypeMembersExpr) }: _*)) }
        }

        log(
          s"""Got refinement $refinement
             |parent=$parent
              |members=$members
              |closestClass=$cls
              |resolvedWeakMembers=${weakTypeMemberSubstitutions.map(_._1)}
              |""".stripMargin
        )
        '{ Tag.refinedTag[T](${ cls }, List(${ parentLtt }), ${ resolvedStructLtt }, Map.empty) }

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
