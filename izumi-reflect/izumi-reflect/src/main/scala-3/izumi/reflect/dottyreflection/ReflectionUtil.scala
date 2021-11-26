package izumi.reflect.dottyreflection

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

}
