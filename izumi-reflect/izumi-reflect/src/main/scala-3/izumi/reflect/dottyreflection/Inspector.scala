package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.{LightTypeTagInheritance, LightTypeTagRef}
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.annotation.tailrec
import scala.quoted.Type
import scala.reflect.Selectable.reflectiveSelectable

abstract class Inspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private def next() = new Inspector(shift + 1) { val qctx: Inspector.this.qctx.type = Inspector.this.qctx }

  def buildTypeRef[T <: AnyKind: Type]: AbstractReference = {
    val tpeTree = TypeTree.of[T]
    log(s" -------- about to inspect ${tpeTree.show} --------")
    val res = inspectTypeRepr(TypeRepr.of[T])
    log(s" -------- done inspecting ${tpeTree.show} --------")
    res
  }

  private[dottyreflection] def inspectTypeRepr(tpe0: TypeRepr, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    val tpe = tpe0.dealias.simplified

    tpe match {
      case a: AnnotatedType =>
        inspectTypeRepr(a.underlying)

      case a: AppliedType =>
        a.args match {
          case Nil =>
            makeNameReferenceFromType(a.tycon)
          case _ =>
            // https://github.com/lampepfl/dotty/issues/8520
            val params = a.tycon.typeSymbol.typeMembers
            val zargs = a.args.zip(params)
            val args = zargs.map(next().inspectTypeParam)
            val nameref = makeNameReferenceFromType(a.tycon)
            FullReference(nameref.ref.name, args, prefix = nameref.prefix)
        }

      case l: TypeLambda =>
        val resType = next().inspectTypeRepr(l.resType)
        val paramNames = l.paramNames.map(LambdaParameter(_))
        LightTypeTagRef.Lambda(paramNames, resType)

      case p: ParamRef =>
        makeNameReferenceFromType(p)

      case t: ThisType =>
        next().inspectTypeRepr(t.tref)

      case a: AndType =>
        val elements = flattenInspectAnd(a)
        if (elements.sizeIs == 1) {
          elements.head
        } else {
          IntersectionReference(elements)
        }

      case o: OrType =>
        val elements = flattenInspectOr(o)
        if (elements.sizeIs == 1) {
          elements.head
        } else {
          UnionReference(elements)
        }

      case term: TermRef =>
        makeNameReferenceFromType(term)

      case r: TypeRef =>
        next().inspectSymbol(r.typeSymbol, Some(r))

      case tb: TypeBounds =>
        inspectBounds(outerTypeRef, tb, isParam = false)

      case constant: ConstantType =>
        makeNameReferenceFromType(constant)

      case lazyref if lazyref.getClass.getName.contains("LazyRef") => // upstream bug seems like
        log(s"TYPEREPR UNSUPPORTED: LazyRef occured $lazyref")
        NameReference("???")

      // Matches CachedRefinedType for this ZIO issue https://github.com/zio/zio/issues/6071
      case ref: Refinement =>
        next().inspectTypeRepr(ref.parent)

      case o =>
        log(s"TYPEREPR UNSUPPORTED: $o")
        throw new RuntimeException(s"TYPEREPR, UNSUPPORTED: ${o.getClass} - $o")

    }
  }

  private def inspectBounds(outerTypeRef: Option[qctx.reflect.TypeRef], tb: TypeBounds, isParam: Boolean): AbstractReference = {
    val hi = next().inspectTypeRepr(tb.hi)
    val low = next().inspectTypeRepr(tb.low)
    if (hi == low) {
      hi
    } else {
      val boundaries = if (hi == LightTypeTagInheritance.tpeAny && low == LightTypeTagInheritance.tpeNothing) {
        Boundaries.Empty
      } else {
        Boundaries.Defined(low, hi)
      }

      if (isParam) {
        // Boundaries in parameters always stand for wildcards even though Scala3 eliminates wildcards
        WildcardReference(boundaries)
      } else {
        // Boundaries which are not parameters are named types (e.g. type members) and are NOT wildcards
        // if hi and low boundaries are defined and distinct, type is not reducible to one of them
        val typeSymbol = outerTypeRef.getOrElse(tb).typeSymbol
        val symref = makeNameReferenceFromSymbol(typeSymbol).copy(boundaries = boundaries)
        symref
      }
    }
  }

  private[dottyreflection] def inspectSymbol(symbol: Symbol, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    symbol match {
      case s if s.isClassDef || s.isValDef || s.isBind =>
        makeNameReferenceFromSymbol(symbol)

      case s if s.isTypeDef =>
        log(s"inspectSymbol: Found TypeDef symbol $s")
        val rhs = s._typeRef._underlying
        next().inspectTypeRepr(rhs, outerTypeRef)

      case s if s.isDefDef =>
        // We don't support method types, but if we do in the future,
        // Something like `s.typeRef.translucentSuperType match { case MethodType(_, params, resultType) => (params, resultType) }`
        // should get the result type & params
        log(s"UNEXPECTED METHOD TYPE, METHOD TYPES UNSUPPORTED: $symbol / ${symbol.tree} / ${s.getClass}")
        throw new RuntimeException(s"UNEXPECTED METHOD TYPE, METHOD TYPES UNSUPPORTED: $symbol / ${symbol.tree} / ${s.getClass}")

      case o => // Should not happen according to documentation of `.tree` method
        // still no access to relevant types
        log(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
        throw new RuntimeException(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
    }
  }

  private def inspectTypeParam(tpe: TypeRepr, td: Symbol): TypeParam = {
    val variance = extractVariance(td)
//    TypeParam(inspectTypeRepr(tpe), variance)

    tpe match {
      case t: TypeBounds =>
        TypeParam(inspectBounds(None, t, isParam = true), variance)
//        TypeParam(inspectTypeRepr(t.hi), variance)
      case t: TypeRepr =>
        TypeParam(inspectTypeRepr(t), variance)
    }
  }

  private def extractVariance(t: Symbol): Variance = {
    if (t.flags.is(Flags.Covariant)) {
      Variance.Covariant
    } else if (t.flags.is(Flags.Contravariant)) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }

  private def flattenInspectAnd(and: AndType): Set[AppliedReference] = {
    flattenAnd(and).toSet.map(inspectTypeRepr(_).asInstanceOf[AppliedReference])
  }

  private def flattenInspectOr(or: OrType): Set[AppliedReference] =
    flattenOr(or).toSet.map(inspectTypeRepr(_).asInstanceOf[AppliedReference])

  private[dottyreflection] def makeNameReferenceFromType(t: TypeRepr): NameReference = {
    t match {
      case ref: TypeRef =>
        log(s"make name reference from type $ref termSymbol=${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol)
      case term: TermRef =>
        log(s"make name reference from term $term")
        makeNameReferenceFromSymbol(term.termSymbol)
      case t: ParamRef =>
        NameReference(tpeName = t.binder.asInstanceOf[LambdaType].paramNames(t.paramNum).toString)
      case constant: ConstantType =>
        NameReference(SymName.SymLiteral(constant.constant.value), Boundaries.Empty, prefix = None)
      case ref =>
        log(s"make name reference from what? $ref ${ref.getClass} ${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol)
    }
  }

  private[dottyreflection] def makeNameReferenceFromSymbol(symbol: Symbol): NameReference = {
    def default: NameReference = {
      val (symName, s) = if (symbol.isTerm || symbol.isBind || symbol.isValDef) {
        SymName.SymTermName(symbol.fullName) -> symbol
      } else if (symbol.flags.is(Flags.Module)) { // Handle ModuleClasses (can creep in from ThisType)
        SymName.SymTermName(symbol.companionModule.fullName) -> symbol.companionModule
      } else {
        SymName.SymTypeName(symbol.fullName) -> symbol
      }
      val prefix = getPrefixFromDefinitionOwner(s) // FIXME: should get prefix from type qualifier (prefix), not from owner
      NameReference(symName, Boundaries.Empty, prefix)
    }

    if (symbol.isBind) { log(s"inspectSymbol: Found Bind symbol $symbol") }

    if (symbol.isValDef) {
      log(s"inspectSymbol: Found ValDef symbol $symbol")
      symbol._typeRef._underlying match {
        case constant: ConstantType => // constant type vals are aliases to constant types
          makeNameReferenceFromType(constant)
        case t: TermRef => // singleton type vals are aliases to their singleton
          makeNameReferenceFromSymbol(t.termSymbol)
        case other =>
          log(s"inspectSymbol: found UNKNOWN symbol in ValDef $other")
          default
      }
    } else {
      default
    }
  }

  // FIXME: should get prefix from type qualifier (prefix), not from owner
  private def getPrefixFromDefinitionOwner(symbol: Symbol): Option[AppliedReference] = {
    val maybeOwner = symbol.maybeOwner
    if (!maybeOwner.exists || maybeOwner.isNoSymbol || maybeOwner.isPackageDef || maybeOwner.isDefDef || maybeOwner.isTypeDef) {
      None
    } else {
      inspectSymbol(maybeOwner) match {
        case a: AppliedReference =>
          log(s"Constructed prefix=$a from owner=$maybeOwner")
          Some(a)
        case _ =>
          None
      }
    }
  }
//  private def getPrefixFromQualifier(t: TypeRepr): Option[AppliedReference] = {
//    @tailrec def unpack(tpe: TypeRepr): Option[TypeRepr] = tpe match {
//      case t: ThisType => unpack(t.tref)
//      case _: NoPrefix => None
//      case t =>
//        ??? // nonsense below
//        val typeSymbol = t.typeSymbol
//        if (!typeSymbol.exists || typeSymbol.isNoSymbol || typeSymbol.isPackageDef || typeSymbol.isDefDef) {
//          None
//        } else {
//          Some(t)
//        }
//    }
//    unpack(t).flatMap(inspectTypeRepr(_) match {
//      case reference: AppliedReference => Some(reference)
//      case _ => None
//    })
//  }
}
