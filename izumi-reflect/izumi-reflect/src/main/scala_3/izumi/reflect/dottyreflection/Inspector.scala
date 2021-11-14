package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
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

  private[dottyreflection] def inspectTypeRepr(tpe: TypeRepr, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    tpe.dealias match {
      case a: AnnotatedType =>
        inspectTypeRepr(a.underlying)

      case a: AppliedType =>
        a.args match {
          case Nil =>
            makeNameReferenceFromType(a.tycon)
          case o =>
            // https://github.com/lampepfl/dotty/issues/8520
            val params = a.tycon.typeSymbol.memberTypes
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
        next().inspectSymbolTree(r.typeSymbol, Some(r))

      case tb: TypeBounds => // weird thingy
        val hi = next().inspectTypeRepr(tb.hi)
        val low = next().inspectTypeRepr(tb.low)
        if (hi == low) hi
        else {
          // if hi and low boundaries are defined and distinct, type is not reducible to one of them
          val typeSymbol = outerTypeRef.getOrElse(tb).typeSymbol
          makeNameReferenceFromSymbol(typeSymbol).copy(boundaries = Boundaries.Defined(low, hi))
        }

      case constant: ConstantType =>
        val hi = next().inspectTypeRepr(constant.widen) // fixme: shouldn't be necessary, as in Scala 2, but bases comparison fails for some reason
        NameReference(SymName.SymLiteral(constant.constant.value), Boundaries.Defined(hi, hi))

      case lazyref if lazyref.getClass.getName.contains("LazyRef") => // upstream bug seems like
        log(s"TYPEREPR UNSUPPORTED: LazyRef occured $lazyref")
        NameReference("???")

      case o =>
        log(s"TYPEREPR UNSUPPORTED: $o")
        throw new RuntimeException(s"TYPEREPR, UNSUPPORTED: ${o.getClass} - $o")
      // {???}

    }
  }

  private[dottyreflection] def inspectSymbolTree(symbol: Symbol, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    symbol.tree match {
      case c: ClassDef =>
        makeNameReferenceFromSymbol(symbol)
      case t: TypeDef =>
        // FIXME: does not work for parameterized type aliases or non-alias abstract types (wrong kindedness)
        log(s"inspectSymbol: Found TypeDef symbol ${t.show}")
        next().inspectTypeRepr(t.rhs.asInstanceOf[TypeTree].tpe, outerTypeRef)
      case d: DefDef =>
        log(s"inspectSymbol: Found DefDef symbol ${d.show}")
        next().inspectTypeRepr(d.returnTpt.tpe)
      case v: ValDef =>
        log(s"inspectSymbol: Found ValDef symbol ${v.show}")
        NameReference(SymName.SymTermName(symbol.fullName))
      case b: Bind =>
        log(s"inspectSymbol: Found Bind symbol ${b.show}")
        NameReference(SymName.SymTermName(symbol.fullName))
      case o => // Should not happen according to documentation of `.tree` method
        log(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
        throw new RuntimeException(s"SYMBOL TREE, UNSUPPORTED: $symbol / $o / ${o.getClass}")
    }
  }

  private def getPrefixFromDefinitionOwner(symbol: Symbol): Option[AppliedReference] = {
    val maybeOwner = symbol.maybeOwner
    if (!maybeOwner.exists || maybeOwner.isNoSymbol || maybeOwner.isPackageDef || maybeOwner.isDefDef || maybeOwner.isTypeDef) {
      None
    } else {
      inspectSymbolTree(maybeOwner) match {
        case a: AppliedReference =>
          Some(a)
        case _ =>
          None
      }
    }
  }

  private def inspectTypeParam(tpe: TypeRepr, td: Symbol): TypeParam = {
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

  private def makeNameReferenceFromType(t: TypeRepr): NameReference = {
    t match {
      case ref: TypeRef =>
        makeNameReferenceFromSymbol(ref.typeSymbol)
      case term: TermRef =>
        makeNameReferenceFromSymbol(term.termSymbol)
      case t: ParamRef =>
        NameReference(tpeName = t.binder.asInstanceOf[{ def paramNames: List[Object] }].paramNames(t.paramNum).toString)
    }
  }

  private[dottyreflection] def makeNameReferenceFromSymbol(symbol: Symbol): NameReference = {
    val symName = if (symbol.isTerm) SymName.SymTermName(symbol.fullName) else SymName.SymTypeName(symbol.fullName)
    val prefix = getPrefixFromDefinitionOwner(symbol) // FIXME: should get prefix from type qualifier (prefix), not from owner
    NameReference(symName, Boundaries.Empty, prefix)
  }

  private def getPrefixFromQualifier(t: TypeRepr) = {
    @tailrec def unpack(tpe: TypeRepr): Option[TypeRepr] = tpe match {
      case t: ThisType => unpack(t.tref)
      case _: NoPrefix => None
      case t =>
        val typeSymbol = t.typeSymbol
        if (!typeSymbol.exists || typeSymbol.isNoSymbol || typeSymbol.isPackageDef || typeSymbol.isDefDef) {
          None
        } else {
          Some(t)
        }
    }
    unpack(t).flatMap(inspectTypeRepr(_) match {
      case reference: AppliedReference => Some(reference)
      case _ => None
    })
  }
}
