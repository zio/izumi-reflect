/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect.macrortti

import izumi.reflect.DebugProperties
import izumi.reflect.internal.{CollectionCompat, NowarnCompat}
import izumi.reflect.internal.fundamentals.collections.IzCollections._
import izumi.reflect.internal.fundamentals.platform.assertions.IzAssert
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config
import izumi.reflect.internal.fundamentals.platform.strings.IzString._
import izumi.reflect.macrortti.LightTypeTagImpl.{Broken, globalCache}
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymTypeName
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable
import scala.reflect.api.Universe

object LightTypeTagImpl {
  private lazy val globalCache = new java.util.WeakHashMap[Any, AbstractReference]

  /** caching is enabled by default for runtime light type tag creation */
  private[this] lazy val runtimeCacheEnabled: Boolean = {
    System
      .getProperty(DebugProperties.`izumi.reflect.rtti.cache.runtime`).asBoolean()
      .getOrElse(true)
  }

  /** Create a LightTypeTag at runtime for a reflected type */
  def makeLightTypeTag(u: Universe)(typeTag: u.Type): LightTypeTag = {
    ReflectionLock.synchronized {
      val logger = TrivialLogger.make[this.type](config = Config.console)
      new LightTypeTagImpl[u.type](u, withCache = runtimeCacheEnabled, logger).makeFullTagImpl(typeTag)
    }
  }

  private[this] object ReflectionLock

  private[macrortti] sealed trait Broken[T, S] {
    def intersectionComponents: Set[T]
    def decls: Set[S]
    def maybeUnbrokenType: Option[T]
  }

  private[macrortti] object Broken {

    final case class Single[T, S](t: T) extends Broken[T, S] {
      override def intersectionComponents: Set[T] = Set(t)
      override def decls: Set[S] = Set.empty
      override def maybeUnbrokenType: Option[T] = Some(t)
    }

    final case class Compound[T, S](intersectionComponents: Set[T], decls: Set[S]) extends Broken[T, S] {
      override def maybeUnbrokenType: Option[T] = None
    }

  }

}

final class LightTypeTagImpl[U <: Universe with Singleton](
  val u: U,
  withCache: Boolean,
  protected val logger: TrivialLogger
) extends LightTypeTagImplHelpers[U] {

  import u._

  def makeFullTagImpl(tpe0: Type): LightTypeTag = {
    val tpe = Dealias.fullNormDealias(tpe0)

    logger.log(s"Initial mainTpe=$tpe:${tpe.getClass} beforeDealias=$tpe0:${tpe0.getClass}")

    val lttRef = makeRef(tpe)

    val allReferenceComponents = mutable
      .LinkedHashSet
      .newBuilder[Type]
      .++=(allTypeReferencesWithBases(tpe, mutable.HashSet.empty, onlyIndirect = false))
      .result()

    val fullDb = makeFullDb(tpe, allReferenceComponents).toMultimap
    val unappliedDb = makeClassOnlyInheritanceDb(tpe, allReferenceComponents.iterator)

    LightTypeTag(lttRef, fullDb, unappliedDb)
  }

  private[this] def allTypeReferencesWithBases(tpe0: Type, basesTermination: mutable.HashSet[Symbol], onlyIndirect: Boolean): Iterator[Type] = {
    val allReferenceComponents = allTypeReferences(tpe0, basesTermination, onlyIndirect)
    allReferenceComponents.iterator.flatMap {
      component =>
        if (component.typeSymbol != NoSymbol) {
          basesTermination += component.typeSymbol
        }

        Iterator.single(component) ++ tpeBases(component).flatMap {
          case t if basesTermination(t.typeSymbol) => Nil
          case t =>
            allTypeReferencesWithBases(t, basesTermination, onlyIndirect = true)
        }
    }
  }

  @NowarnCompat.nowarn("msg=deprecated")
  private[this] def allTypeReferences(mainTpe: Type, basesTermination: mutable.HashSet[Symbol], onlyIndirect: Boolean): collection.Set[Type] = {
    val tpeDealiased = UniRefinement.breakRefinement(mainTpe, squashHKTRefToPolyTypeResultType = false).maybeUnbrokenType.getOrElse(NoType)
    val inh = mutable.LinkedHashSet.empty[Type]
    extractComponents(mainTpe, inh)

    logger.log(s"Extracted all type references for mainTpe=$mainTpe parts=${inh.iterator.map(t => (t, t.getClass.asInstanceOf[Class[Any]])).toMap.niceList()}")

    if (onlyIndirect) {
      inh.retain {
        t => t != mainTpe && t != tpeDealiased && !basesTermination(t.typeSymbol)
      }
    }

    inh
  }

  private[this] def extractComponents(tpeRaw0: Type, inh: mutable.LinkedHashSet[Type]): Unit = {
    val breakResult = UniRefinement.breakRefinement(tpeRaw0, squashHKTRefToPolyTypeResultType = false)
    val current = breakResult.maybeUnbrokenType
    inh ++= current

    val intersectionExpansions = breakResult.intersectionComponents.iterator.flatMap(collectArgsAndBounds)
    val refinementDeclMembers = breakResult.decls.iterator.flatMap(extractRefinementDeclParts)
    val classTypeMemberParts = breakResult.intersectionComponents.iterator.flatMap(extractClassMemberConcreteTypes)

    val indirectComponents = (intersectionExpansions ++ refinementDeclMembers ++ classTypeMemberParts).filterNot(ignored)
    indirectComponents.foreach {
      t =>
        if (!current.contains(t) && !inh(t)) {
          extractComponents(t, inh)
        }
    }
  }

  private[this] def extractRefinementDeclParts(sym: Symbol): List[Type] = {
    if (sym.isMethod) {
      val m = sym.asMethod
      m.returnType :: m.paramLists.iterator.flatten.map(UniRefinement.typeOfParam).toList
    } else if (sym.isType) {
      UniRefinement.concreteTypesOfTypeMemberOnly(sym)
    } else {
      Nil
    }
  }

  private[this] def extractClassMemberConcreteTypes(comp: Type): Iterator[Type] = {
    extractClassTypeMemberDecls(comp).iterator.flatMap {
      case (decl, sig) => UniRefinement.concreteTypesOfTypeMemberSig(decl, sig)
    }
  }

  private[this] def collectArgsAndBounds(tpeUnexpanded0: Type): Iterator[Type] = {
    val tpeUnexpanded = Dealias.fullNormDealias(tpeUnexpanded0)
    val underlying = tpeUnexpanded match {
      case e: ExistentialTypeApi => e.underlying
      case o => o
    }
    val tparamTypeBoundsAndTypeArgs = doExtractNonParamTypeArgs(tpeUnexpanded, underlying)
    logger.log(s"tparamTypeBoundsAndTypeArgs of $tpeUnexpanded=$tparamTypeBoundsAndTypeArgs")

    Iterator.single(tpeUnexpanded) ++
    tpeUnexpanded.typeArgs.iterator ++
    tparamTypeBoundsAndTypeArgs.iterator
  }

  private[this] def doExtractNonParamTypeArgs(tpeUnexpanded: Type, t: Type): List[Type] = {
    val tpePolyTypeResultType = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(t)
    logger.log(
      s"""Got tpeUnexpanded=$tpeUnexpanded:${tpeUnexpanded.getClass} args=${tpeUnexpanded.typeArgs} params=${tpeUnexpanded.typeParams}
         |tpePolyTypeResultType=$tpePolyTypeResultType:${tpePolyTypeResultType.getClass} args=${tpePolyTypeResultType.typeArgs} params=${tpePolyTypeResultType.typeParams}""".stripMargin
    )
    tpePolyTypeResultType.typeArgs.flatMap(extractSingleTypeArgOrBounds)
  }

  private[this] def extractSingleTypeArgOrBounds(targ0: Type): Seq[Type] = {
    val targ = Dealias.fullNormDealias(targ0)
    val targSym = targ.typeSymbol
    targSym.typeSignature match {
      case t: TypeBoundsApi =>
        Seq(t.hi, t.lo).filterNot(ignored)
      case _ if !targSym.isParameter =>
        Seq(targ0)
      case _ =>
        Seq.empty
    }
  }

  private[this] def makeFullDb(tpe: Type, allReferenceComponents: Iterable[Type]): Iterator[(AbstractReference, AbstractReference)] = {
    val stableBases = makeAppliedBases(tpe, allReferenceComponents.iterator)
    val basesAsLambdas = makeLambdaOnlyBases(tpe, allReferenceComponents.iterator)
    basesAsLambdas.iterator ++ stableBases.iterator
  }

  private[this] def makeAppliedBases(mainTpe: Type, allReferenceComponents: Iterator[Type]): List[(AbstractReference, AbstractReference)] = {
    val appliedBases = allReferenceComponents
      .filterNot(isHKTOrPolyTypeOrResultTypeArtifact)
      .filterNot(isExistentialArtifact)
      .flatMap(comp => buildAppliedComponentBases(mainTpe, comp))
      .filterNot { case (t, parent) => parent == t }
      .toList
    logger.log(s"Computed applied bases for tpe=$mainTpe appliedBases=${appliedBases.toMultimap.niceList()}")
    appliedBases
  }

  private[this] def buildAppliedComponentBases(mainTpe: Type, component: Type): List[(AbstractReference, AbstractReference)] = {
    val tparams = component.etaExpand.typeParams
    val lambdaParams = makeLambdaParams(None, tparams).toMap
    val appliedParents = tpeBases(component).filterNot(isHKTOrPolyTypeOrResultTypeArtifact)
    val componentRef = makeRef(component)

    val directBases = appliedParents.map {
      parentTpe =>
        val parentRef = resolveAppliedParentRef(mainTpe, parentTpe, lambdaParams)
        (componentRef, parentRef)
    }
    val typeMemberBase = makeClassTypeMemberRefinement(component, lambdaParams).map(ref => (componentRef, ref))
    directBases ++ typeMemberBase
  }

  private[this] def resolveAppliedParentRef(
    mainTpe: Type,
    parentTpe: Type,
    lambdaParams: Map[String, SymName.LambdaParamName]
  ): AbstractReference = {
    makeRefTop(parentTpe, terminalNames = lambdaParams, isLambdaOutput = lambdaParams.nonEmpty) match {
      case unapplied: Lambda =>
        if (unapplied.someArgumentsReferenced) {
          unapplied
        } else {
          logger.log(s"No arguments referenced in l=$unapplied, parentTpe=$parentTpe, mainTpe=$mainTpe")
          unapplied.output
        }
      case applied: AppliedReference =>
        applied
    }
  }

  private[this] def makeLambdaOnlyBases(mainTpe: Type, allReferenceComponents: Iterator[Type]): List[(AbstractReference, AbstractReference)] = {
    val unappliedBases = allReferenceComponents.flatMap(processLambdasReturningRefinements).toList
    logger.log(s"Computed lambda only bases for tpe=$mainTpe lambdaBases=${unappliedBases.toMultimap.niceList()}")
    unappliedBases
  }

  private[this] def processLambdasReturningRefinements(tpeRaw0: Type): List[(AbstractReference, AbstractReference)] = {
    val broken = UniRefinement.breakRefinement(tpeRaw0, squashHKTRefToPolyTypeResultType = true)
    assertNoPolyTypeInCompoundIntersection(tpeRaw0, broken)

    broken.intersectionComponents.iterator.flatMap {
      component =>
        val componentAsPolyType = component.etaExpand
        val tparams = componentAsPolyType.typeParams
        if (tparams.isEmpty) {
          Nil
        } else {
          makeLambda(componentAsPolyType, tparams)
        }
    }.toList
  }

  private[this] def assertNoPolyTypeInCompoundIntersection(tpeRaw0: Type, broken: Broken[Type, Symbol]): Unit = {
    IzAssert(
      assertion = broken.maybeUnbrokenType.nonEmpty || !broken.intersectionComponents.exists(_.takesTypeArgs),
      clue = {
        s"""Unexpected intersection contains a PolyType:
           |tpeRaw0 = $tpeRaw0
           |components = ${broken.intersectionComponents.niceList(prefix = "*")}
           |""".stripMargin
      }
    )
  }

  private[this] def makeLambda(componentAsPolyType: Type, tparams: List[Symbol]): List[(AbstractReference, AbstractReference)] = {
    val lambdaParams = makeLambdaParams(None, tparams)
    val parentLambdas = makeLambdaParents(componentAsPolyType, lambdaParams)
    val componentLambda = makeRef(componentAsPolyType)
    IzAssert(componentLambda.isInstanceOf[Lambda])
    parentLambdas.map(componentLambda -> _)
  }

  private[this] def makeLambdaParents(componentPolyType: Type, lambdaParams: List[(String, SymName.LambdaParamName)]): List[AbstractReference] = {
    val allBaseTypes = tpeBases(componentPolyType)
    val paramMap = lambdaParams.toMap
    val lambdaParamsUnpacked = lambdaParams.map(_._2)

    val baseRefs = allBaseTypes.map {
      parentTpe =>
        val reference = makeRefTop(parentTpe, terminalNames = paramMap, isLambdaOutput = false)
        wrapLambdaParent(lambdaParamsUnpacked, reference)
    }
    val polyResult = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(componentPolyType)
    val memberRef = makeClassTypeMemberRefinement(polyResult, paramMap).map(wrapLambdaParent(lambdaParamsUnpacked, _))
    baseRefs ++ memberRef
  }

  private[this] def wrapLambdaParent(lambdaParamsUnpacked: List[SymName.LambdaParamName], reference: AbstractReference): AbstractReference = {
    reference match {
      case l: Lambda =>
        l
      case applied: AppliedReference =>
        val l = Lambda(lambdaParamsUnpacked, applied)
        if (l.someArgumentsReferenced) l else applied
    }
  }

  private[this] def makeClassOnlyInheritanceDb(mainTpe: Type, allReferenceComponents: Iterator[Type]): Map[NameReference, Set[NameReference]] = {
    val baseclassReferences = allReferenceComponents
      .flatMap(UniRefinement.breakRefinement(_, squashHKTRefToPolyTypeResultType = true).intersectionComponents)
      .flatMap(extractUnappliedComponentBases)

    val unparameterizedInheritanceData = baseclassReferences
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.collect { case r: AppliedNamedReference => r.asName }.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)

    logger.log(s"Computed unparameterized inheritance data for tpe=$mainTpe unappliedBases=${unparameterizedInheritanceData.toMultimap.niceList()}")
    unparameterizedInheritanceData
  }

  private[this] def extractUnappliedComponentBases(component: Type): List[(NameReference, AbstractReference)] = {
    val prefix = makePrefixReference(component)
    val componentRef = makeNameReference(component, Boundaries.Empty, prefix)
    val appliedBases = tpeBases(component).filterNot(isHKTOrPolyType)
    appliedBases.map(componentRef -> makeRef(_))
  }

  override protected def makeRef(tpe: Type): AbstractReference = {
    if (withCache) {
      globalCache.synchronized(globalCache.get(tpe)) match {
        case null =>
          val ref = makeRefTop(tpe, terminalNames = Map.empty, isLambdaOutput = false)
          globalCache.synchronized(globalCache.put(tpe, ref))
          ref
        case ref =>
          ref
      }
    } else {
      makeRefTop(tpe, terminalNames = Map.empty, isLambdaOutput = false)
    }
  }

  private[this] def makeRefTop(tpe: Type, terminalNames: Map[String, SymName.LambdaParamName], isLambdaOutput: Boolean): AbstractReference = {
    val ctx = RefBuildCtx(0, tpe, Set(tpe), terminalNames, Set.empty)
    makeRefImpl(ctx, tpe, isLambdaOutput)
  }

  private[this] def makeRefImpl(ctx: RefBuildCtx, tpe0: Type, isLambdaOutput: Boolean): AbstractReference = {
    tpe0 match {
      case l if isLambdaOutput =>
        IzAssert(!isHKTOrPolyType(l), l -> l.getClass)
        Lambda(ctx.terminalNames.values.toList, unpackAsProperType(ctx, l))

      case l: PolyTypeApi =>
        unpackLambda(ctx, l)

      case l if l.takesTypeArgs =>
        if (ctx.terminalNames.contains(l.typeSymbol.fullName)) {
          unpackAsProperType(ctx, l)
        } else {
          unpackLambda(ctx, l)
        }

      case e: ExistentialType if e.quantified.headOption.contains(e.underlying.typeSymbol) =>
        WildcardReference(makeBoundaries(ctx, e.underlying))

      case c =>
        unpackProperTypeRefinement(ctx, c)
    }
  }

  private[this] def makeBoundaries(ctx: RefBuildCtx, t: Type): Boundaries = {
    val tOrTypeSymBounds = t match {
      case b: TypeBoundsApi => b
      case _ => t.typeSymbol.typeSignature
    }
    tOrTypeSymBounds match {
      case b: TypeBoundsApi if !isTrivialOrRecursiveBounds(ctx, b) =>
        val lo = makeRefSub(ctx, b.lo)
        val hi = makeRefSub(ctx, b.hi)
        Boundaries.Defined(lo, hi)
      case _ =>
        Boundaries.Empty
    }
  }

  private[this] def isTrivialOrRecursiveBounds(ctx: RefBuildCtx, b: TypeBoundsApi): Boolean = {
    (b.lo =:= nothing && b.hi =:= any) || ctx.nestedIn.contains(b.lo) || ctx.nestedIn.contains(b.hi)
  }

  private[this] def makeRefSub(ctx: RefBuildCtx, tpe: Type): AbstractReference = {
    makeRefSubWithWildcards(ctx, tpe, Set.empty)
  }

  private[this] def makeRefSubWithWildcards(
    ctx: RefBuildCtx,
    tpe: Type,
    knownWildcardsSub: Set[Symbol]
  ): AbstractReference = {
    val allWildcards = ctx.knownWildcards ++ knownWildcardsSub
    if (allWildcards.contains(tpe.typeSymbol)) {
      WildcardReference(makeBoundaries(ctx, ctx.ambientTpe))
    } else {
      makeRefImpl(ctx.advance(tpe, Map.empty, allWildcards), tpe, isLambdaOutput = false)
    }
  }

  private[this] def unpackLambda(ctx: RefBuildCtx, t: TypeApi): AbstractReference = {
    val thisLevel = logger.sub(ctx.level)
    val polyType = t.etaExpand
    val polyTypeResult = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(polyType)

    val tparams = polyType.typeParams
    val nestingLevel = if (ctx.level > 0) Some(ctx.level) else None
    val lambdaParams = makeLambdaParams(nestingLevel, tparams)

    thisLevel.log(s"λ type $t has parameters $lambdaParams and result $polyTypeResult terminal names = ${ctx.terminalNames}")
    val nextCtx = ctx.advance(polyTypeResult, lambdaParams.toMap, ctx.knownWildcards)
    val reference = if (nextCtx.knownWildcards.contains(polyTypeResult.typeSymbol)) {
      WildcardReference(makeBoundaries(ctx, ctx.ambientTpe))
    } else {
      makeRefImpl(nextCtx, polyTypeResult, isLambdaOutput = false)
    }
    val out = Lambda(lambdaParams.map(_._2), reference)
    thisLevel.log(s"Restored λ $t => ${out.longNameWithPrefix}")
    out
  }

  private[this] def unpackProperTypeRefinement(ctx: RefBuildCtx, t0: Type): AppliedReference = {
    IzAssert(!isHKTOrPolyType(Dealias.fullNormDealias(t0)))

    UniRefinement.breakRefinement(t0, squashHKTRefToPolyTypeResultType = false) match {
      case Broken.Compound(components, decls) =>
        val parts = components.map(unpackAsProperType(ctx, _): AppliedReference)
        val intersection = LightTypeTagRef.maybeIntersection(parts)
        if (decls.nonEmpty) {
          Refinement(intersection, decls.flatMap(convertDecl(ctx, _)))
        } else {
          intersection
        }

      case Broken.Single(t) =>
        unpackAsProperType(ctx, t)
    }
  }

  private[this] def unpackAsProperType(ctx: RefBuildCtx, tpeRaw: Type): AppliedNamedReference = {
    val tpe = Dealias.fullNormDealias(tpeRaw)
    val prefix = makePrefixReference(tpe)
    val typeSymbol = tpe.typeSymbol
    val boundaries = makeBoundaries(ctx, tpe)

    val nameRef = ctx.terminalNames.get(typeSymbol.fullName) match {
      case Some(lambdaParameter) =>
        NameReference(lambdaParameter, boundaries, prefix)
      case None =>
        makeNameReference(tpe, boundaries, prefix)
    }

    tpe.typeArgs match {
      case Nil =>
        nameRef
      case args =>
        val tparams = Dealias.fullNormDealias(tpeRaw).typeConstructor.typeParams
        val refParams = buildRefTypeParams(ctx, tpeRaw, args.zip(tparams))
        FullReference(nameRef.asName.ref, refParams, prefix)
    }
  }

  private[this] def buildRefTypeParams(ctx: RefBuildCtx, tpeRaw: Type, zipped: List[(Type, Symbol)]): List[TypeParam] = {
    tpeRaw match {
      case t: ExistentialTypeApi =>
        val quantifiedParams = t.quantified.toSet
        t.underlying.typeArgs.zip(zipped.map(_._2)).map {
          case (arg, param) =>
            val paramRef = resolveExistentialTypeArg(ctx, arg, quantifiedParams)
            TypeParam(paramRef, makeVariance(param.asType))
        }
      case _ =>
        zipped.map {
          case (arg, param) =>
            val paramRef = makeRefSub(ctx, arg)
            TypeParam(paramRef, makeVariance(param.asType))
        }
    }
  }

  private[this] def resolveExistentialTypeArg(ctx: RefBuildCtx, arg: Type, quantifiedParams: Set[Symbol]): AbstractReference = {
    if (quantifiedParams.contains(arg.typeSymbol) && !ctx.terminalNames.contains(arg.typeSymbol.fullName)) {
      WildcardReference(makeBoundaries(ctx, arg))
    } else {
      makeRefSubWithWildcards(ctx, arg, quantifiedParams)
    }
  }

  private[this] def convertDecl(ctx: RefBuildCtx, decl: Symbol): CollectionCompat.IterableOnce[RefinementDecl] = {
    if (decl.isMethod) {
      convertMethodDecl(ctx, decl.asMethod)
    } else if (decl.isType) {
      Some(convertTypeMemberSig(ctx, decl, decl.typeSignature))
    } else {
      None
    }
  }

  private[this] def convertMethodDecl(ctx: RefBuildCtx, declMethod: MethodSymbol): List[RefinementDecl.Signature] = {
    val returnTpe = declMethod.returnType
    val paramLists0 = declMethod.paramLists.map(_.map {
      param =>
        val paramTpe = UniRefinement.typeOfParam(param)
        makeRefSub(ctx, paramTpe).asInstanceOf[AppliedReference]
    })
    val paramLists = if (paramLists0.nonEmpty) paramLists0 else List(Nil)
    val returnRef = makeRefSub(ctx, returnTpe).asInstanceOf[AppliedReference]
    val methodName = declMethod.name.decodedName.toString
    paramLists.map(parameterList => RefinementDecl.Signature(methodName, parameterList, returnRef))
  }

  private[this] def convertTypeMemberSig(ctx: RefBuildCtx, decl: Symbol, sig: Type): RefinementDecl.TypeMember = {
    val tpe = UniRefinement.typeOfTypeMemberSig(decl, sig)
    val declName = decl.name.decodedName.toString
    val ref = makeRefSub(ctx, tpe) match {
      case n @ NameReference(SymTypeName("<none>"), _, _) => n.copy(ref = SymTypeName(declName))
      case other => other
    }
    RefinementDecl.TypeMember(declName, ref)
  }

  private[this] def makeClassTypeMemberRefinement(
    tpe: Type,
    terminalNames: Map[String, SymName.LambdaParamName]
  ): Option[Refinement] = {
    val memberDecls = extractClassTypeMemberDecls(tpe)
    if (memberDecls.isEmpty) {
      None
    } else {
      val ctx = RefBuildCtx(0, tpe, Set(tpe), terminalNames, Set.empty)
      val decls = memberDecls.map {
        case (decl, sig) => convertTypeMemberSig(ctx, decl, sig): RefinementDecl
      }.toSet
      Some(Refinement(LightTypeTagInheritance.tpeObject, decls))
    }
  }

}
