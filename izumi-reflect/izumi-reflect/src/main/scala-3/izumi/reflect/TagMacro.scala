package izumi.reflect

import scala.quoted.{Expr, Quotes, Type}

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.dottyreflection.{Inspect, InspectorBase}

object TagMacro {
  def createTagExpr[A <: AnyKind: Type](using Quotes): Expr[Tag[A]] =
    new TagMacro().createTagExpr[A]
}

final class TagMacro(using override val qctx: Quotes) extends InspectorBase {
  import qctx.reflect._

  override def shift: Int = 0

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val typeRepr = TypeRepr.of[A]
    if (allPartsStrong(typeRepr)) {
      // NB: this should always work, but currently causes `TestModel$::ApplePaymentProvider is not a type lambda, it cannot be parameterized` test failure
      // There's probably a missing case where a higher-kinded type is not a type lambda, with splicing inbetween causing fixup by the compiler
      //   val ltt = Inspect.inspectAny[A]
      val ltt = '{ Inspect.inspect[A] }
      '{ Tag[A](classOf[Any], $ltt) }
    } else {
      summonCombinedTag[A](typeRepr)
    }
  }

  private def mkLtt(typeRepr: TypeRepr): Expr[LightTypeTag] =
    if (allPartsStrong(typeRepr))
      typeRepr.asType match {
//        case given Type[a] => Inspect.inspectAny[a]
        case given Type[a] => '{ Inspect.inspect[a] }
      }
    else {
      val result = summonCombinedTag(typeRepr)
      '{ $result.tag }
    }

  private def summonCombinedTag[T <: AnyKind: Type](typeRepr: TypeRepr): Expr[Tag[T]] =
    typeRepr.dealias match {

      case x if x.typeSymbol.isTypeParam => summonTag(x)

      case AppliedType(ctor, args) =>
        val ctorTag = createTagExpr(ctor.asType)
        val argsTags = Expr.ofList(args.map(mkLtt))
        '{ Tag.appliedTag[T]($ctorTag, $argsTags) }

      case andType: AndType =>
        val ltts0: List[Expr[LightTypeTag]] = flattenAnd(andType).map(mkLtt)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(ltts0)
        andType.asType match {
          case given Type[a] =>
//            val structLtt = Inspect.inspectAny[a]
            val structLtt = '{ Inspect.inspect[a] }
            '{ Tag.refinedTag[T](classOf[Any], $ltts, $structLtt, Map.empty) }
        }

      case orType: OrType =>
        val ltts0: List[Expr[LightTypeTag]] = flattenOr(orType).map(mkLtt)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(ltts0)
        '{ Tag.unionTag[T](classOf[Any], $ltts) }

      case _ =>
        throw new Exception(s"Unsupported type: $typeRepr")
    }

  private def summonTag[any <: AnyKind](typeRepr: TypeRepr): Expr[Tag[any]] =
    typeRepr.asType match {
      case given Type[a] =>
        val message = s"Cannot find implicit Tag[${Type.show[a]}]"
        Expr.summon[Tag[a]].getOrElse(report.errorAndAbort(message)).asInstanceOf[Expr[Tag[any]]]
    }

  /**
    * Returns true if the given type contains no type parameters
    * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
    */
  private def allPartsStrong(typeRepr: TypeRepr): Boolean =
    typeRepr.dealias match {
      case x if x.typeSymbol.isTypeParam => false
      case AppliedType(tpe, args) => allPartsStrong(tpe) && args.forall(allPartsStrong)
      case AndType(lhs, rhs) => allPartsStrong(lhs) && allPartsStrong(rhs)
      case OrType(lhs, rhs) => allPartsStrong(lhs) && allPartsStrong(rhs)
      case TypeRef(tpe, name) => allPartsStrong(tpe)
      case TermRef(tpe, name) => allPartsStrong(tpe)
      case ThisType(tpe) => allPartsStrong(tpe)
      case NoPrefix() => true
      case TypeBounds(lo, hi) => allPartsStrong(lo) && allPartsStrong(hi)
      case TypeLambda(_, _, body) => allPartsStrong(body)
      case strange =>
        log(s"Got unknown type component when checking strength: $strange")
        true
    }

}
