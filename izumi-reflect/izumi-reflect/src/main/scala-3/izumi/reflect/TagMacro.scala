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

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val owners = getClassDefOwners(Symbol.spliceOwner)
    val typeRepr = TypeRepr.of[A].dealias
    if (allPartsStrong(owners, typeRepr)) {
      createTag[A](typeRepr)
    } else {
      summonCombinedTag[A](owners, typeRepr)
    }
  }

  private def createTag[A <: AnyKind](typeRepr: TypeRepr): Expr[Tag[A]] = {
    typeRepr.asType match {
      case given Type[a] =>
        // NB: this should always work, but currently causes `TestModel$::ApplePaymentProvider is not a type lambda, it cannot be parameterized` test failure
        // There's probably a missing case where a higher-kinded type is not a type lambda, with splicing inbetween causing fixup by the compiler
        //   val ltt = Inspect.inspectAny[A]
        val ltt = '{ Inspect.inspect[a] }
        val cls = closestClassOfExpr(typeRepr)
        '{ Tag[a]($cls, $ltt) }.asInstanceOf[Expr[Tag[A]]]
    }
  }

  private def summonCombinedTag[T <: AnyKind: Type](owners: Set[Symbol], typeRepr: TypeRepr): Expr[Tag[T]] = {

    def summonLTTAndFastTrackIfNotTypeParam(typeRepr: TypeRepr): Expr[LightTypeTag] = {
      if (allPartsStrong(owners, typeRepr)) {
        typeRepr.asType match {
          //        case given Type[a] => Inspect.inspectAny[a]
          case given Type[a] => '{ Inspect.inspect[a] }
        }
      } else {
        val result = summonTag[T, Any](typeRepr)
        '{ $result.tag }
      }
    }

    def summonTagAndFastTrackIfNotTypeParam(typeRepr: TypeRepr): Expr[Tag[Any]] = {
      if (allPartsStrong(owners, typeRepr)) {
        createTag[Any](typeRepr)
      } else {
        summonTag[T, Any](typeRepr)
      }
    }

    def summonIfNotLambdaParamOf(typeRepr: TypeRepr, lam: TypeRepr): Expr[Option[LightTypeTag]] = {
      if (isLambdaParamOf(typeRepr, lam)) {
        '{ None }
      } else {
        val tag = summonLTTAndFastTrackIfNotTypeParam(typeRepr)
        '{ Some($tag) }
      }
    }

    def isLambdaParamOf(typeRepr: TypeRepr, lam: TypeRepr): Boolean = {
      typeRepr match {
        case ref: ParamRef if ref.binder == lam => true
        case _ => false
      }
    }

    typeRepr.dealias match {
      case outerLambda: TypeLambda =>
        outerLambda.resType match {
          case AppliedType(ctorTpe, typeArgsTpes) =>
            val paramsRange = 0 until outerLambda.paramNames.size
            val isSimpleApplication = typeArgsTpes.collect {
              case ref: ParamRef if ref.binder == outerLambda => ref.paramNum
            } == paramsRange

            val constructorTag = summonTag[T, Any](ctorTpe)
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
                  val lambdaParameter = SymName.LambdaParamName(idxPlusOne, -3, arity)
                  argTpe -> lambdaParameter
              }.toMap

              val usageOrderDistinctNonLambdaArgs = distinctNonParamArgsTypes.map(t => typeArgToLambdaParameterMap(t))
              val declarationOrderLambdaParamArgs = outerLambdaParamArgsTypeParamRefs.map(t => typeArgToLambdaParameterMap(t))
              val completeTail = usageOrderDistinctNonLambdaArgs ::: declarationOrderLambdaParamArgs

              val usages = typeArgsTpes.map(t => TypeParam(NameReference(typeArgToLambdaParameterMap(t)), Variance.Invariant))

              // we give a distinct lambda parameter to the constructor, even if constructor is one of the type parameters
              val firstParamIdx = 0
              assert(completeTail.size + 1 == arity)
              val ctorLambdaParameter = SymName.LambdaParamName(firstParamIdx, -3, arity)

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
                val nonParamArgsDealiased = distinctNonParamArgsTypes.map(ReflectionUtil.dealiasSimplifiedFull(_))
                log(s"HK COMPLEX Now summoning tags for args=$nonParamArgsDealiased outerLambdaParams=$outerLambdaParamArgsTypeParamRefs")
                Expr.ofList(
                  nonParamArgsDealiased.map(t => '{ Some(${ summonLTTAndFastTrackIfNotTypeParam(t) }) }) ++ outerLambdaParamArgsTypeParamRefs.map(_ => '{ None })
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
        val ctorTag = summonTagAndFastTrackIfNotTypeParam(ctor)
        val argsTags = Expr.ofList(args.map(summonLTTAndFastTrackIfNotTypeParam))
        '{ Tag.appliedTag[T](${ ctorTag }, ${ argsTags }) }

      case andType: AndType =>
        val tpes = flattenAnd(andType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTTAndFastTrackIfNotTypeParam))
        val cls = Literal(ClassOfConstant(lubClassOf(tpes))).asExprOf[Class[?]]
        val dummyAnyStructLtt = {
          // FIXME add constructor for intersections without the unused on Scala 3 struct type
          Inspect.inspectAny[Any]
        }
        '{ Tag.refinedTag[T](${ cls }, ${ ltts }, ${ dummyAnyStructLtt }, Map.empty) }

      case orType: OrType =>
        val tpes = flattenOr(orType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTTAndFastTrackIfNotTypeParam))
        val cls = Literal(ClassOfConstant(lubClassOf(tpes))).asExprOf[Class[?]]
        '{ Tag.unionTag[T](${ cls }, ${ ltts }) }

      case refinement: Refinement =>
        val (members, parent) = flattenRefinements(refinement)
        val cls = closestClassOfExpr(parent)
        val parentLtt = summonLTTAndFastTrackIfNotTypeParam(parent)

        val (allTypeMembers, termMembers) = members.partitionMap {
          case (s, n, tb: TypeBounds) if allPartsStrong(owners, tb) => Left(Left((n, tb)))
          case (_, n, TypeBounds(lo, hi)) if lo == hi => Left(Right((n, hi)))
          case (_, _, tb @ TypeBounds(lo, hi)) =>
            report.errorAndAbort(s"TagMacro: resolving type parameters inside type bounds is not supported, got weak types in bounds=${tb.show}, in type=$typeRepr")
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
          Inspect.inspectAny(using withStrongTpesRefinementTypeRepr.asType, qctx)
        }
        val resolvedTypeMemberLtts = weakTypeMembers.map {
          case (name, tpe) => '{ (${ Expr(name) }, ${ summonLTTAndFastTrackIfNotTypeParam(tpe) }) }
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
        report.errorAndAbort(s"Unsupported type in TagMacro.summonCombinedTag: $typeRepr")
    }
  }

  private def closestClassOfExpr(typeRepr: TypeRepr): Expr[Class[?]] = {
    Literal(ClassOfConstant(lubClassOf(intersectionUnionRefinementClassPartsOf(typeRepr)))).asExprOf[Class[?]]
  }

  private def lubClassOf(tpes: List[TypeRepr]): TypeRepr = {
    tpes.map(_.baseClasses) match {
      case h :: t =>
        val bases = h.to(mutable.LinkedHashSet)
        t.foreach(b => bases.filterInPlace(b.to(mutable.HashSet).contains))
        // rely on the fact that .baseClasses returns classes in order from most specific to least, therefore most specific class should be first.
        bases.headOption.getOrElse(defn.AnyClass).typeRef
      // FIXME: below doesn't work, need to treat AnyVals specially
//        bases.find(!_.typeRef.baseClasses.contains(defn.AnyValClass)).getOrElse(defn.AnyClass).typeRef
      case Nil =>
        defn.AnyClass.typeRef
    }
  }

  private def summonTag[T <: AnyKind, A <: AnyKind](typeRepr: TypeRepr)(using outerCombinedType: Type[T]): Expr[Tag[A]] = {
    typeRepr.asType match {
      case given Type[a] =>
        Expr
          .summon[Tag[a]]
          .getOrElse {
            val aStr = Type.show[a]
            val implicitMessage = defaultImplicitError.replace("${T}", aStr)
            val message = s"""Error when creating a combined tag for ${Type.show[T]}, when summoning Tag for part of that type $aStr:
                             |  $implicitMessage
                             |Structure of overall type was: `${TypeRepr.of[T]}`
                             |Structure of part of the type was: `${TypeRepr.of[a]}
                             |Stack trace: ${locally {
                              import java.io.{PrintWriter, StringWriter}
                              val t = new Exception()
                              val sw = new StringWriter()
                              t.printStackTrace(new PrintWriter(sw))
                              sw.toString
                            }}""".stripMargin
            report.errorAndAbort(message)
          }.asInstanceOf[Expr[Tag[A]]]
    }
  }

  private inline val defaultImplicitError =
    "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
}
