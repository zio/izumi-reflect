package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
import scala.quoted.Type
import scala.reflect.Selectable.reflectiveSelectable

abstract class Inspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private def next() = new Inspector(shift + 1) { val qctx: Inspector.this.qctx.type = Inspector.this.qctx }

  def buildTypeRef[T <: AnyKind: Type]: AbstractReference = {
    val uns = TypeTree.of[T]
    log(s" -------- about to inspect ${uns.show} --------")
    val res = inspectTypeRepr(uns.tpe)
    log(s" -------- done inspecting ${uns.show} --------")
    res
  }

  /**
    *  +- TypeRepr -+- NamedType -+- TermRef
    *               |             +- TypeRef
    *               +- ConstantType
    *               +- SuperType
    *               +- Refinement
    *               +- AppliedType
    *               +- AnnotatedType
    *               +- AndOrType -+- AndType
    *               |             +- OrType
    *               +- MatchType
    *               +- ByNameType
    *               +- ParamRef
    *               +- ThisType
    *               +- RecursiveThis
    *               +- RecursiveType
    *               +- LambdaType -+- MethodOrPoly -+- MethodType
    *               |              |                +- PolyType
    *               |              +- TypeLambda
    *               +- MatchCase
    *               +- TypeBounds
    *               +- NoPrefix
    */
  private[dottyreflection] def inspectTypeRepr(tpe0: TypeRepr, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    val tpe = tpe0._fullNormDealiasSimplified
    tpe _exhaustiveMatch {
      case a: AnnotatedType =>
        inspectTypeRepr(a.underlying)

      case a: AppliedType =>
        a.args match {
          case Nil =>
            makeNameReferenceFromType(a.tycon)
          case o =>
            // https://github.com/lampepfl/dotty/issues/8520
            val params = a.tycon.typeSymbol.typeMembers.filter(_.isTypeParam)
            val zargs = a.args.zip(params)

            val args = zargs.map(next().inspectTypeParam(_, _))
            val nameref = makeNameReferenceFromType(a.tycon)
            FullReference(nameref.ref.name, args, prefix = nameref.prefix)
        }

      case l: LambdaType =>
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

      case tref: TypeRef =>
        makeNameReferenceFromType(tref)

      case matchType: MatchType =>
        makeNameReferenceFromType(matchType)

      case matchCase: MatchCase =>
        next().inspectTypeRepr(matchCase.rhs)

      case tb: TypeBounds => // weird thingy
        val hi = next().inspectTypeRepr(tb.hi)
        val low = next().inspectTypeRepr(tb.low)
        if (hi == low) hi
        else {
          // if hi and low boundaries are defined and distinct, type is not reducible to one of them
          makeNameReferenceFromType(outerTypeRef.getOrElse(tb))
            .copy(boundaries = Boundaries.Defined(low, hi))
        }

      case constant: ConstantType =>
        val hi = next().inspectTypeRepr(constant.widen) // fixme: shouldn't be necessary, as in Scala 2, but bases comparison fails for some reason
        NameReference(SymName.SymLiteral(constant.constant.value), Boundaries.Defined(hi, hi))

      // Matches CachedRefinedType for this ZIO issue https://github.com/zio/zio/issues/6071
      case ref: Refinement =>
        next().inspectTypeRepr(ref.parent)

      case superType: SuperType =>
        next().inspectTypeRepr(superType.thistpe)

      case thisType: ThisType =>
        next().inspectTypeRepr(thisType.tref)

      case byName: ByNameType =>
        next().inspectTypeRepr(byName.underlying)

      case recursiveThis: RecursiveThis =>
        next().inspectTypeRepr(recursiveThis.binder.underlying)

      case recursiveType: RecursiveType =>
        next().inspectTypeRepr(recursiveType.underlying)

      case lazyref if lazyref.getClass.getName.contains("LazyRef") => // upstream bug seems like
        log(s"TYPEREPR UNSUPPORTED: LazyRef occured $lazyref")
        NameReference("???")

      case o: NoPrefix =>
        log(s"TYPEREPR UNSUPPORTED: $o")
        throw new RuntimeException(s"TYPEREPR, UNSUPPORTED: ${o.getClass} - $o")
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

    def makeNameReferenceFromSymbol(symbol: Symbol): NameReference = {
      val symName = symNameFromSymbol(symbol)
      val prefix = getPrefixFromQualifier(t)
      NameReference(symName, Boundaries.Empty, prefix)
    }

    t match {
      case term: TermRef =>
        makeNameReferenceFromSymbol(term.termSymbol)
      case param: ParamRef =>
        // fixme: incorrect, should use numbering scheme
        val paramName: String = param.binder.asInstanceOf[LambdaType].paramNames(param.paramNum)
        NameReference(paramName)
      case other =>
        makeNameReferenceFromSymbol(other.typeSymbol)
    }
  }

  private def getPrefixFromQualifier(t0: TypeRepr): Option[AppliedReference] = {
    @tailrec def unpack(tpe: TypeRepr): Option[TypeRepr] = tpe match {
      case t: NamedType =>
        val prefix = t.qualifier
        val prefixSym = prefix.typeSymbol
        if (!prefixSym.exists || prefixSym.isNoSymbol || prefixSym.isPackageDef || prefixSym.isDefDef) {
          None
        } else {
          Some(prefix)
        }
      case t: ThisType =>
        unpack(t.tref)
      case _: NoPrefix =>
        None
      case t =>
        None
    }

    unpack(t0)
      .flatMap(inspectTypeRepr(_) match {
        case reference: AppliedReference => Some(reference)
        case _ => None
      })
    // FIXME disabling prefixes due to test failures
    None
  }

  private def symNameFromSymbol(symbol: Symbol): SymName = {
    if (symbol.isTerm) {
      SymName.SymTermName(symbol.fullName)
    } else {
      SymName.SymTypeName(symbol.fullName)
    }
  }

}
