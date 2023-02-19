package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.{LightTypeTagInheritance, LightTypeTagRef}
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.annotation.{tailrec, targetName}
import scala.quoted.{Quotes, Type}
import scala.reflect.Selectable.reflectiveSelectable

object Inspector {
  class LamParam(val qctx: Quotes)(val name: LambdaParameter, val tpe: qctx.reflect.TypeRepr, val index: Int)
  class LamContext(val qctx: Quotes)(val params: List[LamParam])
}

abstract class Inspector(protected val shift: Int, val context: List[Inspector.LamContext]) extends InspectorBase {
  import qctx.reflect._

  def next(newContext: List[Inspector.LamContext] = Nil): Inspector { val qctx: Inspector.this.qctx.type } = new Inspector(shift + 1, this.context ++ newContext) {
    val qctx: Inspector.this.qctx.type = Inspector.this.qctx
  }

  def nextLam(l: TypeLambda): Inspector { val qctx: Inspector.this.qctx.type } = {
    val paramNames = l.paramNames.map(LambdaParameter(_))
    val params = paramNames.zipWithIndex.map { case (nme, idx) => new Inspector.LamParam(qctx)(nme, l.param(idx), idx) }.toList
    next(List(Inspector.LamContext(qctx)(params)))
  }

  def buildTypeRef[T <: AnyKind: Type]: AbstractReference = {
    val tpeTree = TypeTree.of[T]
    log(s" -------- about to inspect ${tpeTree.show} --------")
    val res = inspectTypeRepr(TypeRepr.of[T])
    log(s" -------- done inspecting ${tpeTree.show} --------")
    res
  }

  private[dottyreflection] def inspectTypeRepr(tpe0: TypeRepr, outerTypeRef: Option[TypeRef] = None): AbstractReference = {
    val tpe = tpe0.dealias.simplified

    if (context.flatMap(_.params.map(_.tpe)).toSet.contains(tpe0)) {
      assert(tpe == tpe0)
      assert(tpe match {
        case p: ParamRef =>
          true
        case _ =>
          false
      })
    }

    tpe match {
      case a: AnnotatedType =>
        inspectTypeRepr(a.underlying)

      case a: AppliedType =>
        a.args match {
          case Nil =>
            makeNameReferenceFromType(a.tycon)
          case _ =>
            val symbolVariances = a.tycon.typeSymbol.typeMembers.map(extractVariance)
            val variances = if (symbolVariances.sizeCompare(a.args) < 0) {
              a.tycon match {
                case typeParamRef: ParamRef =>
                  typeParamRef._underlying match {
                    case TypeBounds(_, hi) =>
                      hi._declaredVariancesIfHKTypeLambda.fold(Nil)(_.map(extractVariance))
                    case _ =>
                      Nil
                  }
                case _ =>
                  Nil
              }
            } else {
              symbolVariances
            }
            val nameRef = makeNameReferenceFromType(a.tycon)
            val args = a
              .args.iterator.zipAll(variances, null.asInstanceOf[TypeRepr], Variance.Invariant).takeWhile(_._1 != null)
              .map(next().inspectTypeParam).toList
            FullReference(ref = nameRef.ref.name, parameters = args, prefix = nameRef.prefix)
        }

      case l: TypeLambda =>
        val inspector = nextLam(l)
        val resType = inspector.inspectTypeRepr(l.resType)
        val paramNames = inspector.context.last.params.map(_.name)
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
        next().inspectSymbol(r.typeSymbol, Some(r), Some(r))

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

  private def inspectBounds(outerTypeRef: Option[TypeRef], tb: TypeBounds, isParam: Boolean): AbstractReference = {
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
        val typeRepr = outerTypeRef.getOrElse(tb)
        val symref = makeNameReferenceFromType(typeRepr).copy(boundaries = boundaries)
        symref
      }
    }
  }

  private[dottyreflection] def inspectSymbol(symbol: Symbol, outerTypeRef: Option[TypeRef], prefixSource: Option[NamedType]): AbstractReference = {
    symbol match {
      case s if s.isClassDef || s.isValDef || s.isBind =>
        makeNameReferenceFromSymbol(symbol, prefixSource)

      case s if s.isTypeDef =>
        log(s"inspectSymbol: Found TypeDef symbol $s")
        val rhs = s.typeRef._underlying
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

  private def inspectTypeParam(tpe: TypeRepr, variance: Variance): TypeParam = {
    tpe match {
      case t: TypeBounds =>
        TypeParam(inspectBounds(None, t, isParam = true), variance)
//        TypeParam(inspectTypeRepr(t.hi), variance)
      case t: TypeRepr =>
        TypeParam(inspectTypeRepr(t), variance)
    }
  }

  private def extractVariance(t: Symbol): Variance = {
    extractVariance(t.flags)
  }

  @targetName("extractVarianceFlags")
  private def extractVariance(flags: Flags): Variance = {
    if (flags.is(Flags.Covariant)) {
      Variance.Covariant
    } else if (flags.is(Flags.Contravariant)) {
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

  private[dottyreflection] def makeNameReferenceFromType(t: TypeRepr, assertContextCorrectness: Boolean = true): NameReference = {
    t match {
      case ref: TypeRef =>
        log(s"make name reference from type $ref termSymbol=${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol, Some(ref))
      case term: TermRef =>
        log(s"make name reference from term $term")
        makeNameReferenceFromSymbol(term.termSymbol, Some(term))
      case t: ParamRef =>
        val paramName = t.binder.asInstanceOf[LambdaType].paramNames(t.paramNum).toString

        if (assertContextCorrectness) {
          assert(context.flatMap(_.params.map(_.tpe)).contains(t), s"${context.flatMap(_.params.map(_.tpe))} must contain $t")
        }

        NameReference(SymName.LambdaParamName(paramName), Boundaries.Empty, None)

      case constant: ConstantType =>
        NameReference(SymName.SymLiteral(constant.constant.value), Boundaries.Empty, prefix = None)
      case ref =>
        log(s"make name reference from what? $ref ${ref.getClass} ${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol, None)
    }
  }

  private[dottyreflection] def makeNameReferenceFromSymbol(symbol: Symbol, prefixSource1: Option[NamedType]): NameReference = {
    def default: NameReference = {
      val (symName, s, prefixSource2) = if (symbol.isTerm || symbol.isBind || symbol.isValDef) {
        (SymName.SymTermName(symbol.fullName), symbol, symbol.termRef)
      } else if (symbol.flags.is(Flags.Module)) { // Handle ModuleClasses (can creep in from ThisType)
        (SymName.SymTermName(symbol.companionModule.fullName), symbol.companionModule, symbol.companionModule.termRef)
      } else {
        (SymName.SymTypeName(symbol.fullName), symbol, symbol.termRef)
      }
      val prefix = getPrefixFromQualifier(prefixSource1.getOrElse(prefixSource2))
      NameReference(symName, Boundaries.Empty, prefix)
    }

    if (symbol.isBind) { log(s"inspectSymbol: Found Bind symbol $symbol") }

    if (symbol.isValDef) {
      log(s"inspectSymbol: Found ValDef symbol $symbol")
      symbol.typeRef._underlying match {
        case constant: ConstantType => // constant type vals are aliases to constant types
          makeNameReferenceFromType(constant)
        case t: TermRef => // singleton type vals are aliases to their singleton
          makeNameReferenceFromSymbol(t.termSymbol, Some(t))
        case other =>
          log(s"inspectSymbol: found UNKNOWN symbol in ValDef $other")
          default
      }
    } else {
      default
    }
  }

  private def getPrefixFromQualifier(tpe: NamedType): Option[AppliedReference] = {
    tpe.qualifier match {
      // Support https://github.com/zio/izumi-reflect/issues/35
      // consider class member's this-prefix to be the defining template, not the most specific prefix from where the call is happening
      case _: ThisType | _: SuperType | _: RecursiveThis =>
        val s = if (!tpe.termSymbol.isNoSymbol) {
          tpe.termSymbol
        } else {
          tpe.typeSymbol
        }

        val maybeOwner = s.maybeOwner
        if (!maybeOwner.exists || maybeOwner.isNoSymbol || maybeOwner.isPackageDef || maybeOwner.isDefDef || maybeOwner.isTypeDef || maybeOwner.isLocalDummy) {
          None
        } else {
          inspectSymbol(maybeOwner, None, None) match {
            case a: AppliedReference =>
              log(s"Constructed prefix=$a from owner=$maybeOwner")
              Some(a)
            case _ =>
              None
          }
        }

      case prefix =>
        val typeSymbol = prefix.typeSymbol
        if (!typeSymbol.exists || typeSymbol.isNoSymbol || typeSymbol.isPackageDef || typeSymbol.isDefDef || typeSymbol.isTypeDef || typeSymbol.isLocalDummy) {
          None
        } else {
          inspectTypeRepr(prefix) match {
            case reference: AppliedReference =>
              log(s"Constructed prefix=$reference from prefix=$prefix")
              Some(reference)
            case _ => None
          }
        }
    }
  }

}
