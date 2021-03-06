package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.quoted.Type
import scala.reflect.Selectable.reflectiveSelectable

abstract class Inspector(protected val shift: Int) extends InspectorBase {
  self =>

  import qctx.reflect._

  private def next() = new Inspector(shift + 1) {
    val qctx: self.qctx.type = self.qctx
  }

  def buildTypeRef[T <: AnyKind: Type]: AbstractReference = {
    val uns = TypeTree.of[T]
    log(s" -------- about to inspect ${uns.show} --------")
    val res = inspectTypeRepr(uns.tpe)
    log(s" -------- done inspecting ${uns.show} --------")
    res
  }

  private[dottyreflection] def fixReferenceName(reference: AbstractReference, typeRepr: TypeRepr): AbstractReference =
    reference match {
      // if boundaries are defined, this is a unique type, and the type name should be fixed to the (dealiased) declaration
      case NameReference(_, boundaries: Boundaries.Defined, _) =>
        val dealiased = typeRepr.dealias.typeSymbol
        NameReference(SymName.SymTypeName(dealiased.name), boundaries, prefixOf(dealiased))
      case x => x
    }

  private[dottyreflection] def inspectTypeRepr(tpe: TypeRepr): AbstractReference = {
    tpe match {
      case a: AppliedType =>
        a.args match {
          case Nil =>
            asNameRef(a.tycon)
          case o =>
            // https://github.com/lampepfl/dotty/issues/8520
            val params = a.tycon.typeSymbol.memberTypes
            val zargs = a.args.zip(params)

            val args = zargs.map {
              case (t, defn) =>
                next().inspectToB(t, defn)
            }
            val nameref = asNameRef(a.tycon)
            FullReference(nameref.ref.name, args, prefix = nameref.prefix)
        }

      case l: TypeLambda =>
        val resType = next().inspectTypeRepr(l.resType)
        val paramNames = l.paramNames.map(LambdaParameter(_))
        LightTypeTagRef.Lambda(paramNames, resType)

      case t: ParamRef =>
        asNameRef(t)

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

      case r: TypeRef =>
        fixReferenceName(next().inspectSymbolTree(r.typeSymbol), r)

      case a: AnnotatedType =>
        next().inspectTypeRepr(a.underlying)

      case tb: TypeBounds => // weird thingy
        val hi = next().inspectTypeRepr(tb.hi)
        val low = next().inspectTypeRepr(tb.low)
        if (hi == low) hi
        // if hi and low boundaries are defined and distinct, type is not reducible to one of them - however at this
        // point the type name isn't available and we need to use a stand-in...
        else NameReference(SymName.SymTypeName(tb.typeSymbol.name), Boundaries.Defined(low, hi))

      case term: TermRef =>
        asNameRef(term)

      case lazyref if lazyref.getClass.getName.contains("LazyRef") => // upstream bug seems like
        log(s"TYPEREPR UNSUPPORTED: LazyRef occured $lazyref")
        NameReference("???")

      case o =>
        log(s"TYPEREPR UNSUPPORTED: $o")
        throw new RuntimeException(s"TTYPE, UNSUPPORTED: ${o.getClass} - $o")
      //???

    }
  }

  private[dottyreflection] def inspectTypeTree(uns: TypeTree): AbstractReference = {
    val symbol = uns.symbol
    log(s" -------- deep inspect ${uns.show} `${uns.symbol}` ${uns.getClass.getName} $uns --------")
    val res = if (false) {
      inspectSymbol(symbol)
    } else {
      inspectTypeRepr(uns.tpe)
    }
    log(s" -------- done deep inspecting ${uns.show} --------")
    res
  }

  private[dottyreflection] def inspectSymbolTree(symbol: Symbol): AbstractReference = {
    symbol.tree match {
      case c: ClassDef =>
        asNameRefSym(symbol)
      case t: TypeDef => // FIXME: does not work for parameterized type aliases or non-alias abstract types
        log(s"inspectSymbol: Found TypeDef symbol ${t.show}")
        next().inspectTypeTree(t.rhs.asInstanceOf[TypeTree])
      case d: DefDef =>
        log(s"inspectSymbol: Found DefDef symbol ${d.show}")
        next().inspectTypeTree(d.returnTpt)
      case v: ValDef =>
        log(s"inspectSymbol: Found ValDef symbol ${v.show}")
        NameReference(v.name)
      case b: Bind =>
        log(s"inspectSymbol: Found Bind symbol ${b.show}")
        NameReference(b.name)
      case o =>
        log(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
        throw new RuntimeException(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
    }
  }

  private def prefixOf(symbol: Symbol): Option[AppliedReference] = {
    val mo = symbol.maybeOwner
    if (!mo.exists || mo.isNoSymbol || mo.isPackageDef) {
      None
    } else {
      inspectSymbolTree(mo) match {
        case a: AppliedReference =>
          Some(a)
        case _ =>
          None
      }
    }
  }

  private def inspectToB(tpe: TypeRepr, td: Symbol): TypeParam = {
    val variance = extractVariance(td)
    tpe match {
      case t: TypeBounds =>
        TypeParam(inspectTypeRepr(t.hi), variance)
      case t: TypeRepr =>
        TypeParam(inspectTypeRepr(t), variance)
    }
  }

  private def extractVariance(t: Symbol) = {
    if (t.flags.is(Flags.Covariant)) {
      Variance.Covariant
    } else if (t.flags.is(Flags.Contravariant)) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }

  private def flattenInspectAnd(and: AndType): Set[AppliedReference] = {
    val (andTypes, otherTypes) =
      and match {
        case AndType(l @ AndType(_, _), r @ AndType(_, _)) =>
          (Set(l, r), Set.empty[TypeRepr])
        case AndType(l @ AndType(_, _), r) =>
          (Set(l), Set(r))
        case AndType(l, r @ AndType(_, _)) =>
          (Set(r), Set(l))
        case AndType(l, r) =>
          (Set.empty[AndType], Set(l, r))
      }
    val andTypeTags = andTypes.flatMap(flattenInspectAnd)
    val otherTypeTags = otherTypes.map(inspectTypeRepr(_).asInstanceOf[AppliedReference])
    andTypeTags ++ otherTypeTags
  }

  private def flattenInspectOr(or: OrType): Set[AppliedReference] = {
    val (orTypes, otherTypes) =
      or match {
        case OrType(l @ OrType(_, _), r @ OrType(_, _)) =>
          (Set(l, r), Set.empty[TypeRepr])
        case OrType(l @ OrType(_, _), r) =>
          (Set(l), Set(r))
        case OrType(l, r @ OrType(_, _)) =>
          (Set(r), Set(l))
        case OrType(l, r) =>
          (Set.empty[OrType], Set(l, r))
      }
    val orTypeTags = orTypes flatMap flattenInspectOr
    val otherTypeTags = otherTypes.map(inspectTypeRepr(_).asInstanceOf[AppliedReference])
    orTypeTags ++ otherTypeTags
  }

  private def asNameRef(t: TypeRepr): NameReference = {
    t match {
      case ref: TypeRef =>
        asNameRefSym(ref.typeSymbol)
      case term: TermRef =>
        asNameRefSym(term.termSymbol)
      case t: ParamRef =>
        NameReference(t.binder.asInstanceOf[{ def paramNames: List[Object] }].paramNames(t.paramNum).toString)
    }
  }

  private[dottyreflection] def asNameRefSym(t: Symbol): NameReference = {
    val prefix = prefixOf(t)
    NameReference(SymName.SymTypeName(t.fullName), prefix = prefix)
  }
}
