package izumi.reflect.dottyreflection

import scala.annotation.{tailrec, unused}
import scala.collection.immutable.Queue
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

  protected def intersectionUnionRefinementClassPartsOf(tpe: TypeRepr): List[TypeRepr] = {
    tpe.dealias match {
      case AndType(lhs, rhs) =>
        intersectionUnionRefinementClassPartsOf(lhs) ++ intersectionUnionRefinementClassPartsOf(rhs)
      case OrType(lhs, rhs) =>
        intersectionUnionRefinementClassPartsOf(lhs) ++ intersectionUnionRefinementClassPartsOf(rhs)
      case refinement: Refinement =>
        intersectionUnionRefinementClassPartsOf(refinement.parent)
      case _ =>
        List(tpe)
    }
  }

  protected def refinementInfoToParts(tpe0: TypeRepr): List[TypeRepr] = {
    tpe0 match {
      case ByNameType(tpe) =>
        refinementInfoToParts(tpe)
      case MethodType(_, args, res) =>
        args.flatMap(refinementInfoToParts) ++ refinementInfoToParts(res)
      case PolyType(_, tbounds, res) =>
        // FIXME we need to do FullDbInspector.inspectTypeReprToFullBases.lambdify/LightTypeTagImpl.makeLambdaOnlyBases.makeLambdaParents
        //   to wrap the unresolved type params in `res` into a lambda.
        //   As is, if type parameters are used in `res`, we'll add lots of trash types into db
        tbounds.flatMap { case TypeBounds(lo, hi) => List(lo, hi) } ++ refinementInfoToParts(res)
      case tpe =>
        List(tpe)
    }
  }

  protected def flattenRefinements(ref: Refinement): (Queue[(String, TypeRepr)], TypeRepr) = {
    val refinementDecl = (ref.name, ref.info)
    ref.parent match {
      case innerRefinement: Refinement =>
        val (innerRefs, nonRefinementParent) = flattenRefinements(innerRefinement)
        (innerRefs :+ refinementDecl, nonRefinementParent)
      case nonRefinementParent =>
        (Queue(refinementDecl), nonRefinementParent)
    }
  }

  protected def allPartsStrong(outerOwnerClassDefs: Set[Symbol], typeRepr: TypeRepr): Boolean = {
    ReflectionUtil.allPartsStrong(using qctx)(shift, outerOwnerClassDefs, Set.empty, typeRepr)
  }

  protected def getClassDefOwners(symbol: Symbol): Set[Symbol] = {
    ReflectionUtil.getClassDefOwners(using qctx)(symbol)
  }

  import ReflectionUtil.reflectiveUncheckedNonOverloadedSelectable

  extension (typeRef: TypeRef | ParamRef) {
    protected def _underlying: TypeRepr = {
      // This works as a substitution for `TypeRef#underlying` call,
      // but I'm not sure if it's a reliable substitution.

//      typeRef.typeSymbol.owner._typeRef.memberType(typeRef.typeSymbol)

      // No, It's not a reliable substitution. When used on a TypeParamRef it returns Any instead of the underlying TypeBounds
      // https://github.com/lampepfl/dotty/issues/15799

//      val underlying = typeRef
//        .getClass.getMethods.collect { case m if m.getName == "underlying" => m }.head.invoke(
//          typeRef,
//          qctx.getClass.getMethods.collect { case m if m.getName == "ctx" => m }.head.invoke(qctx)
//        )
//      underlying.asInstanceOf[TypeRepr]

      typeRef.asInstanceOf[InternalTypeRefOrParamRef].underlying(using qctx._ctx)
    }
  }

  extension (typeRepr: TypeRepr) {
    protected def _declaredVariancesIfHKTypeLambda: Option[List[Flags]] = {
      try {
        Some(typeRepr.asInstanceOf[InternalHKTypeLambda].declaredVariances)
      } catch {
        case _: NoSuchMethodException => None
      }
    }

    protected def _dealiasSimplifiedFull: TypeRepr = ReflectionUtil.dealiasSimplifiedFull(using qctx)(typeRepr)
  }

  extension (qctx: Quotes) {
    def _ctx: InternalContext = qctx.asInstanceOf[{ def ctx: InternalContext }].ctx
  }

  type InternalTypeRefOrParamRef = {
    def underlying(using InternalContext): TypeRepr
  }

  type InternalHKTypeLambda = {
    def declaredVariances: List[Flags]
  }

  opaque type InternalContext = Any

}

private[reflect] object ReflectionUtil {

  @tailrec
  private[reflect] def dealiasSimplifiedFull(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): qctx.reflect.TypeRepr = {
    val res = typeRepr.dealias.simplified
    if (res.asInstanceOf[AnyRef] eq typeRepr.asInstanceOf[AnyRef]) {
      res
    } else {
      dealiasSimplifiedFull(res)
    }
  }

  private[reflect] inline implicit def reflectiveUncheckedNonOverloadedSelectable(x: Any): UncheckedNonOverloadedSelectable = new UncheckedNonOverloadedSelectable(x)

  /**
    * Returns true if the given type contains no type parameters
    * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
    */
  private[reflect] def allPartsStrong(
    using qctx: Quotes
  )(shift: Int,
    outerOwnerClassDefs: Set[qctx.reflect.Symbol],
    outerLambdas: Set[qctx.reflect.TypeRepr],
    typeRepr: qctx.reflect.TypeRepr
  ): Boolean = {
    import qctx.reflect.*
    typeRepr.dealias match {
      case x if topLevelWeakType(outerOwnerClassDefs, outerLambdas, x) => false
      case AppliedType(tpe, args) =>
        allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe) && args.forall(allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, _))
      case AndType(lhs, rhs) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lhs) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, rhs)
      case OrType(lhs, rhs) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lhs) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, rhs)
      case TypeRef(tpe, _) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case TermRef(tpe, _) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case ThisType(tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case NoPrefix() => true
      case TypeBounds(lo, hi) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, lo) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, hi)
      case lam @ TypeLambda(_, _, body) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas + lam, body)
      case Refinement(parent, _, tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe) && allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, parent)
      case ByNameType(tpe) => allPartsStrong(shift, outerOwnerClassDefs, outerLambdas, tpe)
      case strange =>
        InspectorBase.log(shift, s"Got unknown type component when checking strength: $strange")
        true
    }
  }

  private[reflect] def topLevelWeakType(
    using qctx: Quotes
  )(outerOwnerClassDefs: Set[qctx.reflect.Symbol],
    outerLambdas: Set[qctx.reflect.TypeRepr],
    typeRepr: qctx.reflect.TypeRepr
  ): Boolean = {
    import qctx.reflect.*
    typeRepr match {
      case x if x.typeSymbol.isTypeParam =>
        x match {
          case t: ParamRef if outerLambdas.contains(t.binder) => false
          case _ => true
        }
      // we regard abstract types like T in trait X { type T; Tag[this.T] } - when we are _inside_ the definition template
      // as 'type parameters' too. So that you could define `implicit def tagForT: Tag[this.T]` and the tag would be resolved
      // to this implicit correctly, instead of generating a useless `X::this.type::T` tag.
      // TODO: Due to https://github.com/lampepfl/dotty/issues/16107 not being fixed we have to make sure we're actually
      //       inside the definition of the this-type prefix to count it as 'weak' - unlike Scala 2 we're not protected
      //       from this-types leaking in and have to carry the owner chain here - until that issue is fixed.
      case x @ TypeRef(ThisType(prefix), _) if x.typeSymbol.isAbstractType && !x.typeSymbol.isClassDef && outerOwnerClassDefs.contains(prefix.typeSymbol) =>
        true
      case _ => false
    }
  }

  private[reflect] def getClassDefOwners(using qctx: Quotes)(symbol: qctx.reflect.Symbol): Set[qctx.reflect.Symbol] = {
    Iterator
      .iterate(symbol) {
        s =>
          val owner = s.owner
          if (owner == null || owner.isNoSymbol || owner == qctx.reflect.defn.RootClass) {
            null.asInstanceOf[qctx.reflect.Symbol]
          } else {
            owner
          }
      }
      .takeWhile(_ ne null)
      .filter(s => s.isClassDef && !s.isAbstractType)
      .toSet
  }

  private[reflect] final class UncheckedNonOverloadedSelectable(private val selectable: Any) extends AnyVal with Selectable {

    inline def selectDynamic(name: String): Any = {
      applyDynamic(name)()
    }

    def applyDynamic(name: String, @unused paramTypes: Class[_]*)(args: Any*): Any = {
      val cls = selectable.getClass
      val method = {
        if (args.isEmpty) {
          cls.getMethod(name)
        } else {
          cls.getMethods.collectFirst { case m if m.getName == name => m } match {
            case Some(m) => m
            case None => throw new NoSuchMethodException(s"No method named `$name` found in class `$cls`")
          }
        }
      }
      method.invoke(selectable, args*)
    }

  }

}
