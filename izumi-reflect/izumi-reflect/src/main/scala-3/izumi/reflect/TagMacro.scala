package izumi.reflect

import scala.quoted.{Expr, Quotes, Type, Varargs}
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef}
import izumi.reflect.dottyreflection.{Inspect, Inspector, InspectorBase, ReflectionUtil, TypeInspections}
import izumi.reflect.macrortti.LightTypeTagRef.{
  AbstractReference,
  AppliedNamedReference,
  AppliedReference,
  AppliedReferenceExceptIntersection,
  AppliedReferenceExceptUnion,
  Boundaries,
  FullReference,
  IntersectionReference,
  NameReference,
  RefinementDecl,
  SymName,
  TypeParam,
  UnionReference,
  Variance,
  WildcardReference
}

import scala.collection.mutable

object TagMacro {
  def createTagExpr[A <: AnyKind: Type](using Quotes): Expr[Tag[A]] =
    new TagMacro().createTagExpr[A]
}

final class TagMacro(using override val qctx: Quotes) extends InspectorBase {
  import qctx.reflect._

  override def shift: Int = 0

  private type WeakSymKey = (SymName, Option[AppliedReference])
  private type WeakSymMap = Map[WeakSymKey, SymName.LambdaParamName]

  private val tagSymbol = Symbol.requiredClass("izumi.reflect.Tag")
  private val tagSymbolTypeRef = tagSymbol.typeRef

  private val tagObjSymbol = Symbol.requiredModule("izumi.reflect.Tag")
  private val tagObjApplyMethodSym = tagObjSymbol.declaredMethod("apply").find(_.paramSymss(1).size == 2).get

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val owners = getClassDefOwners(Symbol.spliceOwner)
    val typeRepr = TypeRepr.of[A]._dealiasSimplifiedFull
    if (allPartsStrong(owners, typeRepr)) {
      createTag[A](typeRepr)
    } else {
      summonCombinedTag[A](owners, typeRepr)
    }
  }

  private def createTag[A <: AnyKind](typeRepr0: TypeRepr)(using Type[A]): Expr[Tag[A]] = {
    val typeRepr = typeRepr0._etaExpandTypeRef
    val ltt = Inspect.inspectTypeRepr(typeRepr)
    val cls = closestClassOfTypeRepr(typeRepr)
    Apply(
      fun = TypeApply(
        fun = Select(qualifier = Ref.term(tagObjSymbol.termRef), symbol = tagObjApplyMethodSym),
        args = List(TypeTree.of[A])
      ),
      args = List(cls.asTerm, ltt.asTerm)
    ).asExpr.asInstanceOf[Expr[Tag[A]]]
  }

  private def summonLTT[T <: AnyKind: Type](typeRepr: TypeRepr): Expr[LightTypeTag] = {
    typeRepr match {
      case TypeBounds(low, high) =>
        val lowTag = summonTag[T](low)
        val highTag = summonTag[T](high)
        '{ LightTypeTag.wildcardType($lowTag.tag, $highTag.tag) }
      case _ =>
        val result = summonTag[T](typeRepr)
        '{ $result.tag }
    }
  }

  private def isLambdaParamOf(typeRepr: TypeRepr, lam: TypeRepr): Boolean = {
    typeRepr match {
      case ref: ParamRef if ref.binder == lam => true
      case _ => false
    }
  }

  private def summonIfNotLambdaParamOf[T <: AnyKind: Type](typeRepr: TypeRepr, lam: TypeRepr): Expr[Option[LightTypeTag]] = {
    if (isLambdaParamOf(typeRepr, lam)) {
      '{ None }
    } else {
      val tag = summonLTT[T](typeRepr)
      '{ Some($tag) }
    }
  }

  private def summonCombinedTag[T <: AnyKind: Type](owners: Set[Symbol], typeReprDealiased: TypeRepr): Expr[Tag[T]] = {
    typeReprDealiased match {
      case outerLambda: TypeLambda =>
        summonTypeLambdaTag[T](outerLambda)

      case AppliedType(ctor, args) =>
        val ctorTag = summonTag[T](ctor)
        val argsTags = Expr.ofList(args.map(summonLTT[T]))
        '{ Tag.appliedTag[T](${ ctorTag }, ${ argsTags }) }

      case andType: AndType =>
        summonIntersectionTag[T](typeReprDealiased, andType)

      case orType: OrType =>
        val tpes = flattenOr(orType)
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTT[T]))
        val cls = Literal(ClassOfConstant(lubClassOf(typeReprDealiased, tpes))).asExpr.asInstanceOf[Expr[Class[?]]]
        '{ Tag.unionTag[T](${ cls }, ${ ltts }) }

      case refinement: Refinement =>
        summonRefinementTag[T](owners, refinement)

      case x if ReflectionUtil.topLevelWeakType(owners, Set.empty, x) =>
        val tStr = x.show
        val implicitMessage = defaultImplicitError.replace("${T}", tStr)
        report.errorAndAbort(s"""$tStr is a type parameter without an implicit Tag!
                                |  $implicitMessage
                                |""".stripMargin)

      case _ =>
        report.errorAndAbort(s"Unsupported type in TagMacro.summonCombinedTag: $typeReprDealiased")
    }
  }

  private def summonIntersectionTag[T <: AnyKind: Type](typeReprDealiased: TypeRepr, andType: AndType): Expr[Tag[T]] = {
    val tpes = flattenAnd(andType)
    val ltts: Expr[List[LightTypeTag]] = Expr.ofList(tpes.map(summonLTT[T]))
    val cls = Literal(ClassOfConstant(lubClassOf(typeReprDealiased, tpes))).asExpr.asInstanceOf[Expr[Class[?]]]
    val dummyAnyStructLtt = Inspect.inspectAny[Any]
    '{ Tag.refinedTag[T](${ cls }, ${ ltts }, ${ dummyAnyStructLtt }, Map.empty) }
  }

  private def summonTypeLambdaTag[T <: AnyKind: Type](outerLambda: TypeLambda): Expr[Tag[T]] = {
    outerLambda.resType match {
      case AppliedType(ctorTpe, typeArgsTpes) =>
        val paramsRange = 0 until outerLambda.paramNames.size
        val isSimpleApplication = typeArgsTpes.collect {
          case ref: ParamRef if ref.binder == outerLambda => ref.paramNum
        } == paramsRange

        if (isSimpleApplication) {
          val constructorTag = summonTag[T](ctorTpe)
          val argsTags = Expr.ofList(typeArgsTpes.map(a => summonIfNotLambdaParamOf[T](a, outerLambda)))
          '{ Tag.appliedTagNonPos[T](${ constructorTag }, ${ argsTags }) }
        } else {
          summonComplexTypeLambdaTag[T](outerLambda, AppliedType(ctorTpe, typeArgsTpes))
        }

      case other =>
        report.warning(
          s"""TODO: Pathological intersection refinement result in lambda being reconstructed result=`${other.show}` in the rhs of type lambda lam=`${outerLambda.show}`
             |Only simple applied types of form F[A] are supported in results of type lambdas. The generated tag will not work correctly.""".stripMargin
        )
        createTag[T](outerLambda)
    }
  }

  private def summonComplexTypeLambdaTag[T <: AnyKind: Type](
    outerLambda: TypeLambda,
    appliedResult: AppliedType
  ): Expr[Tag[T]] = {
    val AppliedType(ctorTpe, typeArgsTpes) = appliedResult
    val constructorTag = summonTag[T](ctorTpe)
    val paramsRange = 0 until outerLambda.paramNames.size
    val distinctNonParamArgsTypes = typeArgsTpes.filter(!isLambdaParamOf(_, outerLambda)).distinct
    val outerLambdaParamArgsTypeParamRefs = paramsRange.map(outerLambda.param(_)).toList

    val arity = 1 + distinctNonParamArgsTypes.size + outerLambdaParamArgsTypeParamRefs.size
    val fullParamTail = (distinctNonParamArgsTypes ++ outerLambdaParamArgsTypeParamRefs).iterator.distinct.zipWithIndex
    val typeArgToLambdaParameterMap = fullParamTail.map {
      case (argTpe, idx) =>
        argTpe -> SymName.LambdaParamName(idx + 1, LightTypeTagRef.LambdaConstants.tagMacro, arity)
    }.toMap

    val usageOrderDistinctNonLambdaArgs = distinctNonParamArgsTypes.map(typeArgToLambdaParameterMap)
    val declarationOrderLambdaParamArgs = outerLambdaParamArgsTypeParamRefs.map(typeArgToLambdaParameterMap)
    val completeTail = usageOrderDistinctNonLambdaArgs ::: declarationOrderLambdaParamArgs
    val usages = typeArgsTpes.map(t => TypeParam(NameReference(typeArgToLambdaParameterMap(t)), Variance.Invariant))

    assert(completeTail.size + 1 == arity)
    val ctorLambdaParameter = SymName.LambdaParamName(0, LightTypeTagRef.LambdaConstants.tagMacro, arity)
    val ctorApplyingLambda = LightTypeTagRef.Lambda(
      ctorLambdaParameter :: completeTail,
      FullReference(ctorLambdaParameter, usages)
    )

    val nonParamArgsDealiased = distinctNonParamArgsTypes.map(_._dealiasSimplifiedFull)
    val argTagsExceptCtor = Expr.ofList(
      nonParamArgsDealiased.map(t => '{ Some(${ summonLTT[T](t) }) }) ++ outerLambdaParamArgsTypeParamRefs.map(_ => '{ None })
    )
    val outerLambdaReprTag = Inspect.makeParsedLightTypeTagImpl(LightTypeTag(ctorApplyingLambda, Map.empty, Map.empty))
    '{
      val ctorTag = ${ constructorTag }.asInstanceOf[Tag[Any]]
      Tag.appliedTagNonPosAux[T](ctorTag.closestClass, ${ outerLambdaReprTag }, Some(ctorTag.tag) :: ${ argTagsExceptCtor })
    }
  }

  private def summonRefinementTag[T <: AnyKind: Type](owners: Set[Symbol], refinement: Refinement): Expr[Tag[T]] = {
    val (members, parent) = flattenRefinements(refinement)
    val cls = closestClassOfTypeRepr(parent)
    val parentLtt = summonLTT[T](parent)

    val (allTypeMembers, termMembers) = members.partitionMap {
      case (_, n, tb: TypeBounds) if allPartsStrong(owners, tb) => Left(Left((n, tb)))
      case (_, n, TypeBounds(lo, hi)) if lo == hi => Left(Right((n, hi)))
      case (_, _, tb @ TypeBounds(_, _)) =>
        report.errorAndAbort(
          s"TagMacro: resolving type parameters inside type bounds is not supported, got weak types in bounds=${tb.show}, in type=$refinement"
        )
      case x => Right(x)
    }
    val (strongTypeBounds, weakTypeMembers) = allTypeMembers.partitionMap(identity)
    val termOnlyRefinementTypeRepr = termMembers.foldRight(defn.AnyRefClass.typeRef: TypeRepr) {
      case ((_, name, tpe), acc) => Refinement(parent = acc, name = name, info = tpe)
    }
    val withStrongTpesRefinementTypeRepr = strongTypeBounds.foldRight(termOnlyRefinementTypeRepr) {
      case ((name, tpe), acc) => Refinement(parent = acc, name = name, info = tpe)
    }
    val structLtt = buildRefinementStructLtt[T](owners, withStrongTpesRefinementTypeRepr)
    val resolvedTypeMemberLtts = weakTypeMembers.map {
      case (name, tpe) => '{ (${ Expr(name) }, ${ summonLTT[T](tpe) }) }
    }
    '{ Tag.refinedTag[T](${ cls }, List(${ parentLtt }), ${ structLtt }, Map(${ Varargs(resolvedTypeMemberLtts) }: _*)) }
  }

  private def buildRefinementStructLtt[T <: AnyKind: Type](
    owners: Set[Symbol],
    structTypeRepr: TypeRepr
  ): Expr[LightTypeTag] = {
    val inspector = Inspector.make(qctx)
    val weakTermTypes = extractWeakTypes(owners -> Set.empty[TypeRepr], structTypeRepr)
      .map(tpe => (inspector.makeNameReferenceFromType(tpe), tpe))
      .foldLeft(Vector.empty[(NameReference, TypeRepr)]) {
        case (acc, elem @ (nameRef, _)) =>
          if (acc.exists(_._1 == nameRef)) acc else acc :+ elem
      }
      .toList
    if (weakTermTypes.isEmpty) {
      Inspect.inspectTypeRepr(structTypeRepr)
    } else {
      buildParameterizedStructLtt[T](structTypeRepr, weakTermTypes)
    }
  }

  private def buildParameterizedStructLtt[T <: AnyKind: Type](
    structTypeRepr: TypeRepr,
    weakTermTypes: List[(NameReference, TypeRepr)]
  ): Expr[LightTypeTag] = {
    val arity = weakTermTypes.size
    val indexedParams = weakTermTypes.zipWithIndex.map {
      case ((nameRef, tpe), idx) =>
        val param = SymName.LambdaParamName(idx, LightTypeTagRef.LambdaConstants.tagMacro, arity)
        ((nameRef.symName, nameRef.prefix), param, tpe)
    }
    val rewriteMap: WeakSymMap = indexedParams.map { case (key, param, _) => key -> param }.toMap
    val lambdaParams = indexedParams.map(_._2)
    val rawRef = TypeInspections(structTypeRepr)
    val fullDb = rewriteFullDb(TypeInspections.fullDb(structTypeRepr), rewriteMap -> lambdaParams)
    val nameDb = TypeInspections.unappliedDb(structTypeRepr).filterNot {
      case (k, _) => rewriteMap.contains((k.symName, k.prefix))
    }
    val lambdaRef = LightTypeTagRef.Lambda(lambdaParams, replaceWeakRefs(rawRef, rewriteMap))
    val lambdaLtt = Inspect.makeParsedLightTypeTagImpl(LightTypeTag(lambdaRef, fullDb, nameDb))
    val resolvedArgs = Expr.ofList(indexedParams.map { case (_, _, tpe) => summonLTT[T](tpe) })
    '{ ${ lambdaLtt }.combine(${ resolvedArgs }: _*) }
  }

  private def rewriteFullDb(
    fullDb: Map[AbstractReference, Set[AbstractReference]],
    ctx: (WeakSymMap, List[SymName.LambdaParamName])
  ): Map[AbstractReference, Set[AbstractReference]] = {
    val (rewriteMap, lambdaParams) = ctx
    val paramSet = lambdaParams.toSet
    fullDb.flatMap {
      case (k: NameReference, _) if rewriteMap.contains((k.symName, k.prefix)) =>
        None
      case (k, vs) =>
        val rk = wrapIfReferencesParams(replaceWeakRefs(k, rewriteMap), lambdaParams)
        val rvs = vs.map(v => wrapIfReferencesParams(replaceWeakRefs(v, rewriteMap), lambdaParams))
        if (paramSet.exists(p => rk == NameReference(p))) None else Some(rk -> rvs)
    }
  }

  private def wrapIfReferencesParams(
    ref: AbstractReference,
    lambdaParams: List[SymName.LambdaParamName]
  ): AbstractReference = {
    ref match {
      case l: LightTypeTagRef.Lambda => l
      case applied: AppliedReference =>
        val candidate = LightTypeTagRef.Lambda(lambdaParams, applied)
        if (candidate.someArgumentsReferenced) candidate else applied
    }
  }

  private def extractWeakTypes(
    scope: (Set[Symbol], Set[TypeRepr]),
    tpe0: TypeRepr
  ): List[TypeRepr] = {
    val (owners, outerLambdas) = scope
    val tpe = tpe0._dealiasSimplifiedFull
    tpe match {
      case x if ReflectionUtil.topLevelWeakType(owners, outerLambdas, x) =>
        List(x)
      case AppliedType(tycon, args) =>
        extractWeakTypes(scope, tycon._dealiasSimplifiedFull) ++ args.flatMap(extractWeakTypes(scope, _))
      case AndType(lhs, rhs) =>
        extractWeakTypes(scope, lhs) ++ extractWeakTypes(scope, rhs)
      case OrType(lhs, rhs) =>
        extractWeakTypes(scope, lhs) ++ extractWeakTypes(scope, rhs)
      case TypeBounds(lo, hi) =>
        extractWeakTypes(scope, lo) ++ extractWeakTypes(scope, hi)
      case Refinement(p, _, info) =>
        extractWeakTypes(scope, p) ++ extractWeakTypes(scope, info)
      case other =>
        extractWeakTypesFromLeafOrLambda(scope, other)
    }
  }

  private def extractWeakTypesFromLeafOrLambda(
    scope: (Set[Symbol], Set[TypeRepr]),
    tpe: TypeRepr
  ): List[TypeRepr] = {
    val (owners, outerLambdas) = scope
    tpe match {
      case TypeRef(prefix, _) => extractWeakTypes(scope, prefix)
      case TermRef(prefix, _) => extractWeakTypes(scope, prefix)
      case ThisType(tref) => extractWeakTypes(scope, tref)
      case ByNameType(underlying) => extractWeakTypes(scope, underlying)
      case lam @ TypeLambda(_, _, body) =>
        extractWeakTypes(owners -> (outerLambdas + lam), body)
      case poly @ PolyType(_, bounds, body) =>
        val nextScope = owners -> (outerLambdas + poly)
        bounds.flatMap(extractWeakTypes(nextScope, _)) ++ extractWeakTypes(nextScope, body)
      case MethodType(_, args, res) =>
        args.flatMap(extractWeakTypes(scope, _)) ++ extractWeakTypes(scope, res)
      case _ =>
        Nil
    }
  }

  private def replaceWeakRefs(ref: AbstractReference, mapping: WeakSymMap): AbstractReference = {
    ref match {
      case LightTypeTagRef.Lambda(input, output) =>
        LightTypeTagRef.Lambda(input, replaceWeakRefs(output, mapping))
      case applied: AppliedReference =>
        replaceWeakApplied(applied, mapping)
    }
  }

  private def replaceWeakApplied(ref: AppliedReference, mapping: WeakSymMap): AppliedReference = {
    ref match {
      case IntersectionReference(refs) =>
        IntersectionReference(refs.map(r => replaceWeakApplied(r, mapping).asInstanceOf[AppliedReferenceExceptIntersection]))
      case UnionReference(refs) =>
        UnionReference(refs.map(r => replaceWeakApplied(r, mapping).asInstanceOf[AppliedReferenceExceptUnion]))
      case WildcardReference(boundaries) =>
        WildcardReference(replaceWeakBounds(boundaries, mapping))
      case LightTypeTagRef.Refinement(base, decls) =>
        val rdecls = decls.map(replaceWeakDecl(_, mapping))
        LightTypeTagRef.Refinement(replaceWeakApplied(base, mapping), rdecls)
      case named: AppliedNamedReference =>
        replaceWeakNamed(named, mapping)
    }
  }

  private def replaceWeakDecl(decl: RefinementDecl, mapping: WeakSymMap): RefinementDecl = {
    decl match {
      case RefinementDecl.Signature(name, input, output) =>
        RefinementDecl.Signature(name, input.map(replaceWeakApplied(_, mapping)), replaceWeakApplied(output, mapping))
      case RefinementDecl.TypeMember(name, tpeRef) =>
        RefinementDecl.TypeMember(name, replaceWeakRefs(tpeRef, mapping))
    }
  }

  private def replaceWeakNamed(ref: AppliedNamedReference, mapping: WeakSymMap): AppliedNamedReference = {
    ref match {
      case NameReference(sym, boundaries, prefix) =>
        mapping.get((sym, prefix)) match {
          case Some(param) => NameReference(param, Boundaries.Empty, None)
          case None => NameReference(sym, replaceWeakBounds(boundaries, mapping), prefix.map(replaceWeakApplied(_, mapping)))
        }
      case FullReference(sym, parameters, prefix) =>
        val newParams = parameters.map(p => TypeParam(replaceWeakRefs(p.ref, mapping), p.variance))
        mapping.get((sym, prefix)) match {
          case Some(param) => FullReference(param, newParams, None)
          case None => FullReference(sym, newParams, prefix.map(replaceWeakApplied(_, mapping)))
        }
    }
  }

  private def replaceWeakBounds(boundaries: Boundaries, mapping: WeakSymMap): Boundaries = {
    boundaries match {
      case Boundaries.Defined(bottom, top) =>
        Boundaries.Defined(replaceWeakRefs(bottom, mapping), replaceWeakRefs(top, mapping))
      case Boundaries.Empty =>
        Boundaries.Empty
    }
  }

  private def closestClassOfTypeRepr(typeRepr: TypeRepr): Expr[Class[?]] = {
    Literal(ClassOfConstant(lubClassOf(typeRepr, intersectionUnionRefinementClassPartsOf(typeRepr)))).asExpr.asInstanceOf[Expr[Class[?]]]
  }

  private def lubClassOf(specificTpe: TypeRepr, tpes: List[TypeRepr]): TypeRepr = {
    tpes.map(_.baseClasses) match {
      case h :: t =>
        val bases = h.to(mutable.LinkedHashSet)
        t.foreach(b => bases.filterInPlace(b.to(mutable.HashSet)))
        val baseClass = bases.headOption.getOrElse(defn.AnyClass)
        specificTpe.baseType(baseClass)
      case Nil =>
        defn.AnyClass.typeRef
    }
  }

  private def summonTag[T <: AnyKind](typeRepr: TypeRepr)(using outerCombinedType: Type[T]): Expr[Tag[?]] = {
    val tagTypeRepr = AppliedType(tagSymbolTypeRef, List(typeRepr))
    Implicits.search(tagTypeRepr) match {
      case s: ImplicitSearchSuccess =>
        s.tree.asExpr.asInstanceOf[Expr[Tag[?]]]
      case _: ImplicitSearchFailure =>
        val aStr = typeRepr.show
        val implicitMessage = defaultImplicitError.replace("${T}", aStr)
        val message = s"""Error when creating a combined tag for ${Type.show[T]}, when summoning Tag for part of that type $aStr:
                         |  $implicitMessage
                         |Structure of overall type was: `${TypeRepr.of[T]}`
                         |Structure of part of the type was: `$typeRepr
                         |Stack trace: ${locally {
                          import java.io.{PrintWriter, StringWriter}
                          val t = new Exception()
                          val sw = new StringWriter()
                          t.printStackTrace(new PrintWriter(sw))
                          sw.toString
                        }}""".stripMargin
        report.errorAndAbort(message)
    }
  }

  private inline val defaultImplicitError =
    "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
}
