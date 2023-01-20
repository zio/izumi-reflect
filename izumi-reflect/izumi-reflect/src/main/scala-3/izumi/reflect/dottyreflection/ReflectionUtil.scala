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

  protected def intersectionUnionClassPartsOf(tpe: TypeRepr): List[TypeRepr] = {
    tpe.dealias match {
      case AndType(lhs, rhs) =>
        intersectionUnionClassPartsOf(lhs) ++ intersectionUnionClassPartsOf(rhs)
      case OrType(lhs, rhs) =>
        intersectionUnionClassPartsOf(lhs) ++ intersectionUnionClassPartsOf(rhs)
      case refinement: Refinement =>
        intersectionUnionClassPartsOf(refinement.parent)
      case _ =>
        List(tpe)
    }
  }

  protected def allPartsStrong(typeRepr: TypeRepr): Boolean = {
    ReflectionUtil.allPartsStrong(using qctx)(shift, typeRepr)
  }

  extension (typeRef: TypeRef | ParamRef) {
    protected def _underlying: TypeRepr = {
      // This works as a substitution for `TypeRef#underlying` call,
      // but I'm not sure if it's a reliable substitution.

//      typeRef.typeSymbol.owner._typeRef.memberType(typeRef.typeSymbol)

      // No, It's not a reliable substitution. When used on a TypeParamRef it returns Any instead of the underlying TypeBounds
      // https://github.com/lampepfl/dotty/issues/15799

      val underlying = typeRef
        .getClass.getMethods.collect { case m if m.getName == "underlying" => m }.head.invoke(
          typeRef,
          qctx.getClass.getMethods.collect { case m if m.getName == "ctx" => m }.head.invoke(qctx)
        )
      underlying.asInstanceOf[TypeRepr]
    }
  }

  extension (typeRepr: TypeRepr) {
    protected def _declaredVariancesIfHKTypeLambda: Option[List[Flags]] = {
      val maybeMethod = typeRepr.getClass.getMethods.collectFirst { case m if m.getName == "declaredVariances" => m }
      maybeMethod.map(_.invoke(typeRepr).asInstanceOf[List[Flags]])
    }
  }

}

object ReflectionUtil {

  /**
    * Returns true if the given type contains no type parameters
    * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
    */
  private[reflect] def allPartsStrong(using qctx: Quotes)(shift: Int, typeRepr: qctx.reflect.TypeRepr): Boolean = {
    import qctx.reflect.*
    typeRepr.dealias match {
      case x if topLevelWeakType(x) => false
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

  private[reflect] def topLevelWeakType(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Boolean = {
    import qctx.reflect.*
    typeRepr match {
      case x if x.typeSymbol.isTypeParam => true
      case x @ TypeRef(ThisType(_), _) if x.typeSymbol.isAbstractType && !x.typeSymbol.isClassDef => true
      case _ => false
    }
  }

}
