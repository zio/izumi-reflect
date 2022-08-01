package izumi.reflect.dottyreflection

import scala.quoted.Quotes

private[dottyreflection] trait ReflectionUtil { this: InspectorBase =>

  import qctx.reflect._

  protected def flattenAnd(tpe: TypeRepr): List[TypeRepr] =
    tpe.dealias match {
      case AndType(lhs, rhs) => flattenAnd(lhs) ++ flattenAnd(rhs)
      case _ => List(tpe)
    }

  protected def flattenOr(tpe: TypeRepr): List[TypeRepr] =
    tpe.dealias match {
      case OrType(lhs, rhs) => flattenOr(lhs) ++ flattenOr(rhs)
      case _ => List(tpe)
    }

  protected def allPartsStrong(typeRepr: TypeRepr): Boolean = {
    ReflectionUtil.allPartsStrong(using qctx)(shift, typeRepr)
  }

}

object ReflectionUtil {

  /**
    * Returns true if the given type contains no type parameters
    * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
    */
  private[reflect] def allPartsStrong(using q: Quotes)(shift: Int, typeRepr: q.reflect.TypeRepr): Boolean = {
    import q.reflect.*
    typeRepr.dealias match {
      case x if x.typeSymbol.isTypeParam => false
      case x @ TypeRef(ThisType(_), _) if x.typeSymbol.isAbstractType && !x.typeSymbol.isClassDef => false
      case AppliedType(tpe, args) => allPartsStrong(shift, tpe) && args.forall(allPartsStrong(shift, _))
      case AndType(lhs, rhs) => allPartsStrong(shift, lhs) && allPartsStrong(shift, rhs)
      case OrType(lhs, rhs) => allPartsStrong(shift, lhs) && allPartsStrong(shift, rhs)
      case TypeRef(tpe, _) => allPartsStrong(shift, tpe)
      case TermRef(tpe, _) => allPartsStrong(shift, tpe)
      case ThisType(tpe) => allPartsStrong(shift, tpe)
      case NoPrefix() => true
      case TypeBounds(lo, hi) => allPartsStrong(shift, lo) && allPartsStrong(shift, hi)
      case TypeLambda(_, _, body) => allPartsStrong(shift, body)
      case strange =>
        InspectorBase.log(shift, s"Got unknown type component when checking strength: $strange")
        true
    }
  }

}
