package izumi.reflect

import scala.quoted.{Expr, Quotes, Type}

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.dottyreflection.{Inspect, InspectorBase, ReflectionUtil}

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
    def summonLttIfTypeParam(typeRepr: TypeRepr): Expr[LightTypeTag] = {
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

    def summonTagIfTypeParam[A](typeRepr: TypeRepr): Expr[Tag[A]] = {
      if (allPartsStrong(typeRepr)) {
        createTag[A](typeRepr)
      } else {
        summonTag[T, A](typeRepr)
      }
    }

    def summonIfNotParameter(typeRepr: TypeRepr, lam: TypeRepr): Expr[Option[LightTypeTag]] = {
      typeRepr match {
        case ref: ParamRef if ref.binder == lam =>
          '{ None }
        case _ =>
          val tag = summonLttIfTypeParam(typeRepr)
          '{ Some($tag) }
      }
    }

    typeRepr.dealias match {
      case lam: TypeLambda =>
        lam.resType match {
          case AppliedType(ctor, args) =>
            val ctorTag = summonTagIfTypeParam(ctor)
            val argsTags = Expr.ofList(args.map(a => summonIfNotParameter(a, lam)))
            '{ Tag.appliedTagNonPos[T]($ctorTag, $argsTags) }

          case r =>
            throw new Exception(s"TODO: Pathological lambda being reconstructed: $typeRepr")
        }

      case AppliedType(ctor, args) =>
        val ctorTag = summonTagIfTypeParam(ctor)
        val argsTags = Expr.ofList(args.map(summonLttIfTypeParam))
        '{ Tag.appliedTag[T]($ctorTag, $argsTags) }

      case andType: AndType =>
        val tpes = flattenAnd(andType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLttIfTypeParam))
        val cls = Literal(ClassOfConstant(lubClassOf(tpes))).asExprOf[Class[?]]
        val structLtt = Inspect.inspectAny(using andType.asType, qctx)
        '{ Tag.refinedTag[T]($cls, $ltts, $structLtt, Map.empty) }

      case orType: OrType =>
        val tpes = flattenOr(orType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLttIfTypeParam))
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
      case x if ReflectionUtil.topLevelWeakType(x) =>
        val tStr = x.show
        val implicitMessage = defaultImplicitError.replace("${T}", tStr)
        report.errorAndAbort(s"""$tStr is a type parameter without an implicit Tag!
                                |  $implicitMessage
                                |""".stripMargin)

      case _ =>
        throw new Exception(s"Unsupported type in TagMacro.summonCombinedTag: $typeRepr")
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

  private def summonTag[T <: AnyKind, A <: AnyKind](typeRepr: TypeRepr)(using Type[T]): Expr[Tag[A]] = {
    typeRepr.asType match {
      case given Type[a] =>
        val aStr = Type.show[a]
        val implicitMessage = defaultImplicitError.replace("${T}", aStr)
        val message = s"""Error when creating a combined tag for ${Type.show[T]} (${TypeRepr.of[T]}), when summoning Tag for part of that type $aStr:
                         |  $implicitMessage
                         |""".stripMargin
        Expr.summon[Tag[a]].getOrElse(report.errorAndAbort(message)).asInstanceOf[Expr[Tag[A]]]
    }
  }

  private inline val defaultImplicitError =
    "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
}
