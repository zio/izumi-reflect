package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.{LightTypeTagInheritance, LightTypeTagRef}
import izumi.reflect.macrortti.LightTypeTagRef.*
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymTypeName

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.Queue
import scala.quoted.{Quotes, Type}
import scala.reflect.Selectable.reflectiveSelectable

object Inspector {
  case class LamParam(name: String, index: Int, depth: Int, arity: Int)(val qctx: Quotes)(val tpe: qctx.reflect.TypeRepr) {
    def asParam = SymName.LambdaParamName(index, depth, arity) // s"$depth:$index/$arity")
    // this might be useful for debugging
    // def asParam = LambdaParameter(s"$depth:$index/$arity:$name")

    override def toString: String = s"[($name: $tpe) = $asParam]"
  }
  case class LamContext(params: List[LamParam])

  def make(q: Quotes): Inspector { val qctx: q.type } = new Inspector(0, Queue.empty) {
    override val qctx: q.type = q
  }
}

abstract class Inspector(protected val shift: Int, val context: Queue[Inspector.LamContext]) extends InspectorBase {
  import qctx.reflect._

  def next(newContext: List[Inspector.LamContext] = Nil): Inspector { val qctx: Inspector.this.qctx.type } = new Inspector(shift + 1, this.context ++ newContext) {
    val qctx: Inspector.this.qctx.type = Inspector.this.qctx
  }

  def nextLam(lambda: TypeLambda): Inspector { val qctx: Inspector.this.qctx.type } = {
    val params = lambda
      .paramNames
      .zipWithIndex
      .map {
        case (nme, idx) =>
          Inspector.LamParam(nme, idx, context.size, lambda.paramNames.size)(qctx)(lambda.param(idx))
      }
      .toList
    next(List(Inspector.LamContext(params)))
  }

  def buildTypeRef[T <: AnyKind: Type]: AbstractReference = {
    log(s" -------- about to inspect ${TypeTree.of[T].show} (${TypeRepr.of[T]}) --------")
    val res = inspectTypeRepr(TypeRepr.of[T])
    log(s" -------- done inspecting ${TypeTree.of[T].show} (${TypeRepr.of[T]}) --------")
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

      case appliedType: AppliedType =>
        appliedType.args match {
          case Nil =>
            makeNameReferenceFromType(appliedType.tycon)
          case _ =>
            val symbolVariances = appliedType.tycon.typeSymbol.typeMembers.map(extractVariance)
            val variances = if (symbolVariances.sizeCompare(appliedType.args) < 0) {
              appliedType.tycon match {
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
            val nameRef = makeNameReferenceFromType(appliedType.tycon)
            val args = appliedType
              .args.iterator
              .zipAll(variances, null.asInstanceOf[TypeRepr], Variance.Invariant)
              .takeWhile(_._1 != null)
              .map(next().inspectTypeParam(_, _)).toList
            FullReference(symName = nameRef.symName, parameters = args, prefix = nameRef.prefix)
        }

      case l: TypeLambda =>
        val inspector = nextLam(l)
        val resType = inspector.inspectTypeRepr(l.resType)
        val paramNames = inspector.context.last.params.map(_.asParam)
        LightTypeTagRef.Lambda(paramNames, resType)

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

      case p: ParamRef =>
        makeNameReferenceFromType(p)

      case term: TermRef =>
        makeNameReferenceFromType(term)

      case r: TypeRef =>
        next().inspectSymbol(r.typeSymbol, Some(r), Some(r))

      case tb: TypeBounds =>
        inspectBounds(outerTypeRef, tb)

      case constant: ConstantType =>
        makeNameReferenceFromType(constant)

      case ref: Refinement =>
        next().inspectRefinements(ref)

      case lazyref if lazyref.getClass.getName.contains("LazyRef") => // upstream bug seems like
        log(s"TYPEREPR UNSUPPORTED: LazyRef occured $lazyref")
        NameReference(SymName.SymTypeName("???"))

      case o =>
        log(s"TYPEREPR UNSUPPORTED: $o")
        throw new RuntimeException(s"TYPEREPR, UNSUPPORTED: ${o.getClass} - $o")

    }
  }

  private def inspectRefinements(ref: Refinement): AbstractReference = {
    val (refinements, nonRefinementParent) = flattenRefinements(ref)

    val parentRef = next().inspectTypeRepr(nonRefinementParent)

    val refinementDecls = refinements.map {
      case (name, ByNameType(tpe)) => // def x(): Int
        RefinementDecl.Signature(name, Nil, next().inspectTypeRepr(tpe).asInstanceOf[AppliedReference])

      case (name, m0: MethodOrPoly) => // def x(i: Int): Int; def x[A](a: A): A
        // FIXME as of version 2.3.0 RefinementDecl.Signature model is broken
        //   it doesn't support either multiple parameter lists or type parameters
        //   on methods, so this is a hacky to avoid fixing that for now.
        def squashMethodIgnorePolyType(m: TypeRepr): (List[TypeRepr], TypeRepr) = {
          m match {
            case p: PolyType =>
              squashMethodIgnorePolyType(p.resType)
            case m: MethodType =>
              val inputs = m.paramTypes
              val (inputs2, res) = squashMethodIgnorePolyType(m.resType)
              (inputs ++ inputs2, res)
            case tpe =>
              (Nil, tpe)
          }
        }
        val (inputTpes, resType) = squashMethodIgnorePolyType(m0)
        val inputRefs = inputTpes.map(next().inspectTypeRepr(_).asInstanceOf[AppliedReference])
        val outputRef = next().inspectTypeRepr(resType).asInstanceOf[AppliedReference]
        RefinementDecl.Signature(name, inputRefs, outputRef)

      case (name, bounds: TypeBounds) => // type T = Int
        val boundaries = next().inspectBoundsImpl(bounds)
        val ref = boundaries match {
          case Left(ref) =>
            // concrete type member: type T = Int
            ref
          case Right(definedBoundaries) =>
            // abstract type member: type T >: Int <: AnyVal
            NameReference(SymName.SymTypeName(name), definedBoundaries, None)
        }
        RefinementDecl.TypeMember(name, ref)

      case (name, tpe) => // val t: Int
        RefinementDecl.Signature(name, Nil, next().inspectTypeRepr(tpe).asInstanceOf[AppliedReference])
    }

    val ohOh = parentRef.asInstanceOf[AppliedReference]

    LightTypeTagRef.Refinement(ohOh, refinementDecls.toSet)
  }

  private def inspectBounds(outerTypeRef: Option[TypeRef], tb: TypeBounds): AbstractReference = {
    log(s"inspectBounds: found TypeBounds $tb outer=$outerTypeRef")
    val res = inspectBoundsImpl(tb) match {
      case Left(hi) =>
        hi
      case Right(boundaries) =>
        if (outerTypeRef.isEmpty) {
          // Boundaries in parameters always stand for wildcards even though Scala3 eliminates wildcards
          WildcardReference(boundaries)
        } else {
          // Boundaries which are not parameters are named types (e.g. type members) and are NOT wildcards
          // if hi and low boundaries are defined and distinct, type is not reducible to one of them
          val typeRepr = outerTypeRef.get
          val symref = makeNameReferenceFromType(typeRepr).copy(boundaries = boundaries)
          symref
        }
    }
    invertTypeMemberWithTypeLambdaBounds(res)
  }

  private def invertTypeMemberWithTypeLambdaBounds(abstractReference: AbstractReference): AbstractReference = abstractReference match {
    case NameReference(symName, Boundaries.Defined(bottom @ _, LightTypeTagRef.Lambda(input, realTop @ _)), prefix) =>
      // We throw away both upper and lower boundaries
      // Upper boundaries we'll recover later in fulldb and inheritancedb
      // But lower boundaries we don't recover
      LightTypeTagRef.Lambda(input, FullReference(symName, input.map(p => TypeParam(NameReference(p), Variance.Invariant)), prefix))
    case other => other
  }

  private def inspectBoundsImpl(tb: TypeBounds): Either[AbstractReference, Boundaries] = {
    val hi = next().inspectTypeRepr(tb.hi)
    val low = next().inspectTypeRepr(tb.low)
    if (hi == low) {
      Left(hi)
    } else {
      val boundaries = if (hi == LightTypeTagInheritance.tpeAny && low == LightTypeTagInheritance.tpeNothing) {
        Boundaries.Empty
      } else {
        Boundaries.Defined(low, hi)
      }
      Right(boundaries)
    }
  }

  private[dottyreflection] def inspectSymbol(symbol: Symbol, outerTypeRef: Option[TypeRef], prefixSource: Option[NamedType]): AbstractReference = {
    symbol match {
      case s if s.isClassDef || s.isValDef || s.isBind =>
        log(s"inspectSymbol: Found Cls=${s.isClassDef} Val=${s.isValDef} Bind=${s.isBind} symbol $s")
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
      case t: TypeBounds => // wildcard
        TypeParam(inspectBounds(outerTypeRef = None, tb = t), variance)
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

  private[dottyreflection] def makeNameReferenceFromType(t: TypeRepr): NameReference = {
    t match {
      case ref: TypeRef =>
        log(s"make name reference from type $ref termSymbol=${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol, Some(ref))
      case term: TermRef =>
        log(s"make name reference from term $term")
        makeNameReferenceFromSymbol(term.termSymbol, Some(term))
      case t: ParamRef =>
        log(s"make name reference from paramRef $t")
        val isInContext = context.flatMap(_.params.map(_.tpe)).contains(t)
        if (isInContext) {
          // assert(isInContext, s"${context.flatMap(_.params.map(_.tpe)).map(t => t)} must contain $t")

          val candidates = context.reverse.flatMap(_.params).filter(_.tpe == t)
          val contextParam = candidates.head

          locally {
            val paramName = t.binder.asInstanceOf[LambdaType].paramNames(t.paramNum).toString
            assert(contextParam.name == paramName, s"$contextParam should match $paramName")
          }

          NameReference(contextParam.asParam, Boundaries.Empty, None)

        } else {
          val lt = t.binder.asInstanceOf[LambdaType]
          NameReference(SymName.LambdaParamName(t.paramNum, -1, lt.paramNames.size), Boundaries.Empty, None)
        }

      case constant: ConstantType =>
        log(s"make name reference from ConstantType $constant")
        NameReference(SymName.SymLiteral(constant.constant.value), Boundaries.Empty, prefix = None)

      case ref =>
        log(s"make name reference from what? $ref ${ref.getClass} ${ref.termSymbol}")
        makeNameReferenceFromSymbol(ref.typeSymbol, None)
    }
  }

  @tailrec
  private[dottyreflection] final def makeNameReferenceFromSymbol(symbol: Symbol, prefixSource1: Option[NamedType]): NameReference = {
    def default: NameReference = {
      val (symName, prefixSource2) = if (symbol.isTerm || symbol.isBind || symbol.isValDef) {
        (SymName.SymTermName(symbol.fullName), symbol.termRef)
      } else if (symbol.flags.is(Flags.Module)) { // Handle ModuleClasses (can creep in from ThisType)
        (SymName.SymTermName(symbol.companionModule.fullName), symbol.companionModule.termRef)
      } else {
        (SymName.SymTypeName(symbol.fullName), symbol.termRef)
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
