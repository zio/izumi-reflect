package izumi.reflect

import scala.quoted.{Expr, Quotes, Type}
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef}
import izumi.reflect.dottyreflection.{Inspect, InspectorBase, ReflectionUtil}
import izumi.reflect.macrortti.LightTypeTagRef.{FullReference, LambdaParameter, NameReference, SymName, TypeParam, Variance}

import scala.collection.mutable

object TagMacro {
  def createTagExpr[A <: AnyKind: Type](using Quotes): Expr[Tag[A]] =
    new TagMacro().createTagExpr[A]
}

final class TagMacro(using override val qctx: Quotes) extends InspectorBase {
  import qctx.reflect._

  override def shift: Int = 0

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val typeRepr = TypeRepr.of[A].dealias
    if (allPartsStrong(typeRepr)) {
      createTag[A](typeRepr)
    } else {
      summonCombinedTag[A](typeRepr)
    }
  }

  private def createTag[A <: AnyKind](typeRepr: TypeRepr): Expr[Tag[A]] = {
    typeRepr.asType match {
      case given Type[a] =>
        // NB: this should always work, but currently causes `TestModel$::ApplePaymentProvider is not a type lambda, it cannot be parameterized` test failure
        // There's probably a missing case where a higher-kinded type is not a type lambda, with splicing inbetween causing fixup by the compiler
        //   val ltt = Inspect.inspectAny[A]
        val ltt = '{ Inspect.inspect[a] }
        val cls = Literal(ClassOfConstant(lubClassOf(intersectionUnionClassPartsOf(typeRepr)))).asExprOf[Class[?]]
        '{ Tag[a]($cls, $ltt) }.asInstanceOf[Expr[Tag[A]]]
    }
  }

  private def summonCombinedTag[T <: AnyKind: Type](typeRepr: TypeRepr): Expr[Tag[T]] = {
    def summonLTTAndFastTrackIfNotTypeParam(typeRepr: TypeRepr): Expr[LightTypeTag] = {
      if (allPartsStrong(typeRepr)) {
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
      if (allPartsStrong(typeRepr)) {
        createTag[Any](typeRepr)
      } else {
        summonTag[T, Any](typeRepr)
      }
    }

    def summonIfNotParameterOf(typeRepr: TypeRepr, lam: TypeRepr): Expr[Option[LightTypeTag]] = {
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
              val argsTags = Expr.ofList(typeArgsTpes.map(a => summonIfNotParameterOf(a, outerLambda)))
              '{ Tag.appliedTagNonPos[T](${ constructorTag }, ${ argsTags }) }
            } else {
              val distinctNonParamArgsTypes = typeArgsTpes.filter(!isLambdaParamOf(_, outerLambda)).distinct
              val outerLambdaParamArgsTypeParamRefs = paramsRange.map(outerLambda.param(_)).toList

              // we give a distinct lambda parameter to the constructor, even if constructor is one of the type parameters
              val ctorLambdaParameter = LambdaParameter("0")

              val typeArgToLambdaParameterMap = (distinctNonParamArgsTypes ++ outerLambdaParamArgsTypeParamRefs)
                .iterator.distinct.zipWithIndex.map {
                  case (argTpe, idx) =>
                    val idxPlusOne = idx + 1
                    val lambdaParameter = LambdaParameter(s"$idxPlusOne")
                    argTpe -> lambdaParameter
                }.toMap

              val usageOrderDistinctNonLambdaArgs = distinctNonParamArgsTypes.map(t => typeArgToLambdaParameterMap(t))
              val declarationOrderLambdaParamArgs = outerLambdaParamArgsTypeParamRefs.map(t => typeArgToLambdaParameterMap(t))

              val usages = typeArgsTpes.map(t => TypeParam(NameReference(SymName.LambdaParamName(typeArgToLambdaParameterMap(t).name)), Variance.Invariant))

              val ctorApplyingLambda =
                LightTypeTagRef.Lambda(
                  ctorLambdaParameter :: usageOrderDistinctNonLambdaArgs ::: declarationOrderLambdaParamArgs,
                  FullReference(SymName.LambdaParamName(ctorLambdaParameter.name), usages)
                )

              log(s"""HK non-trivial lambda construction:
                     |ctorApplyingLambda=$ctorApplyingLambda
                     |usageOrderNonLambdaArgs=$usageOrderDistinctNonLambdaArgs
                     |declarationOrderLambdaParamArgs=$declarationOrderLambdaParamArgs
                     |""".stripMargin)

              val argTagsExceptCtor = {
                val nonParamArgsDealiased = distinctNonParamArgsTypes.map(_.dealias.simplified)
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
        '{ Tag.appliedTag[T]($ctorTag, $argsTags) }

      case andType: AndType =>
        val tpes = flattenAnd(andType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTTAndFastTrackIfNotTypeParam))
        val cls = Literal(ClassOfConstant(lubClassOf(tpes))).asExprOf[Class[?]]
        val structLtt = Inspect.inspectAny(using andType.asType, qctx)
        '{ Tag.refinedTag[T]($cls, $ltts, $structLtt, Map.empty) }

      case orType: OrType =>
        val tpes = flattenOr(orType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTTAndFastTrackIfNotTypeParam))
        val cls = Literal(ClassOfConstant(lubClassOf(tpes))).asExprOf[Class[?]]
        '{ Tag.unionTag[T]($cls, $ltts) }

      case refinement: Refinement =>
        log(
          s"""Unsupported refinement $refinement
             |parent=${refinement.parent} name=${refinement.name} info=${refinement.info}""".stripMargin
        )
        val tag = summonTag[T, Any](refinement.parent)
        '{ $tag.asInstanceOf[Tag[T]] }

      // error: the entire type is just a proper type parameter with no type arguments
      // it cannot be resolved further
      case x if ReflectionUtil.topLevelWeakType(Set.empty, x) =>
        val tStr = x.show
        val implicitMessage = defaultImplicitError.replace("${T}", tStr)
        report.errorAndAbort(s"""$tStr is a type parameter without an implicit Tag!
                                |  $implicitMessage
                                |""".stripMargin)

      case _ =>
        report.errorAndAbort(s"Unsupported type in TagMacro.summonCombinedTag: $typeRepr")
    }
  }

  private def lubClassOf(tpes: List[TypeRepr]): TypeRepr = {
    tpes.map(_.baseClasses) match {
      case h :: t =>
        val bases = h.to(mutable.LinkedHashSet)
        t.foreach(b => bases.filterInPlace(b.to(mutable.HashSet)))
        // rely on the fact that .baseClasses returns classes in order from most specific to list, therefore most specific class should be first.
        bases.headOption.getOrElse(defn.AnyClass).typeRef
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
