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
import izumi.reflect.macrortti.LightTypeTagImpl.globalCache
import izumi.reflect.macrortti.LightTypeTagImplSupport.Broken
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymLiteral, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
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
}

final class LightTypeTagImpl[U <: Universe with Singleton](
  override val u: U,
  withCache: Boolean,
  logger: TrivialLogger
) extends LightTypeTagImplSupport[U](u, logger) {

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
    val tpeDealiased = dealiasPrepare(mainTpe).maybeUnbrokenType.getOrElse(NoType)
    val inh = mutable.LinkedHashSet.empty[Type]
    extractComponents(mainTpe, inh)

    logger.log(s"Extracted all type references for mainTpe=$mainTpe parts=${inh.iterator.map(t => (t, t.getClass.asInstanceOf[Class[Any]])).toMap.niceList()}")

    if (onlyIndirect) {
      inh.retain(t => t != mainTpe && t != tpeDealiased && !basesTermination(t.typeSymbol))
    }
    inh
  }

  @inline private[this] def dealiasPrepare(t0: Type): Broken[Type, Symbol] = {
    UniRefinement.breakRefinement(t0, squashHKTRefToPolyTypeResultType = false)
  }

  private[this] def extractComponents(tpeRaw0: Type, inh: mutable.LinkedHashSet[Type]): Unit = {
    val breakResult = dealiasPrepare(tpeRaw0)
    val current = breakResult.maybeUnbrokenType
    inh ++= current

    val intersectionExpansionsArgsBoundsIter = breakResult.intersectionComponents.iterator.flatMap(collectArgsAndBounds)
    val refinementDeclMembersIter = breakResult.decls.iterator.flatMap(extractDeclMemberTypes)
    val indirectComponents = intersectionExpansionsArgsBoundsIter ++ refinementDeclMembersIter

    indirectComponents.foreach {
      t =>
        val alreadyVisited = current.contains(t) || inh(t)
        if (!alreadyVisited && !ignored(t)) {
          extractComponents(t, inh)
        }
    }
  }

  private[this] def extractDeclMemberTypes(sym: Symbol): List[Type] = {
    if (sym.isMethod) {
      val m = sym.asMethod
      m.returnType :: m.paramLists.iterator.flatten.map(UniRefinement.typeOfParam).toList
    } else if (sym.isType) {
      UniRefinement.concreteTypesOfTypeMemberOnly(sym)
    } else {
      Nil
    }
  }

  private[this] def collectArgsAndBounds(tpeUnexpanded0: Type): Iterator[Type] = {
    val tpeUnexpanded = Dealias.fullNormDealias(tpeUnexpanded0)
    val underlying = tpeUnexpanded match {
      case e: ExistentialTypeApi => e.underlying
      case o => o
    }
    val tparamTypeBoundsAndTypeArgs = extractNonParamTypeArgs(tpeUnexpanded, underlying)
    val classMemberTypes = extractClassMemberComponentTypes(tpeUnexpanded)

    logger.log(s"tparamTypeBoundsAndTypeArgs of $tpeUnexpanded=$tparamTypeBoundsAndTypeArgs")

    Iterator.single(tpeUnexpanded) ++
    tpeUnexpanded.typeArgs.iterator ++
    tparamTypeBoundsAndTypeArgs.iterator ++
    classMemberTypes.iterator
  }

  private[this] def extractNonParamTypeArgs(tpeUnexpanded: Type, t: Type): List[Type] = {
    val tpePolyTypeResultType = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(t)

    logger.log(
      s"""Got tpeUnexpanded=$tpeUnexpanded:${tpeUnexpanded.getClass} args=${tpeUnexpanded.typeArgs} params=${tpeUnexpanded.typeParams}
         |tpePolyTypeResultType=$tpePolyTypeResultType:${tpePolyTypeResultType.getClass} args=${tpePolyTypeResultType.typeArgs} params=${tpePolyTypeResultType.typeParams}""".stripMargin
    )

    tpePolyTypeResultType.typeArgs.flatMap(extractSingleTypeArgBounds)
  }

  private[this] def extractSingleTypeArgBounds(targ0: Type): Seq[Type] = {
    val targ = Dealias.fullNormDealias(targ0)
    val targSym = targ.typeSymbol
    targSym.typeSignature match {
      case t: TypeBoundsApi =>
        Seq(t.hi, t.lo).filterNot(ignored)
      case _ =>
        if (!targSym.isParameter) Seq(targ0) else Seq.empty
    }
  }

  private[this] def buildClassTypeMemberDecls(
    targetTpe: Type,
    terminalNames: Map[String, SymName.LambdaParamName]
  ): Set[RefinementDecl] = {
    val ctx = RefContext(0, Set(targetTpe), terminalNames, Set.empty)
    collectClassTypeMembers(targetTpe).iterator.map {
      case (declName, memberSig) =>
        val ref = makeRefSub(ctx, memberSig, Map.empty) match {
          case n @ NameReference(SymTypeName("<none>"), _, _) => n.copy(ref = SymTypeName(declName))
          case other => other
        }
        RefinementDecl.TypeMember(declName, ref): RefinementDecl
    }.toSet
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
      .flatMap(extractAppliedComponentBases)
      .filterNot { case (t, parent) => parent == t }
      .toList
    logger.log(s"Computed applied bases for tpe=$mainTpe appliedBases=${appliedBases.toMultimap.niceList()}")
    appliedBases
  }

  private[this] def extractAppliedComponentBases(component: Type): List[(AbstractReference, AbstractReference)] = {
    val tparams = component.etaExpand.typeParams
    val lambdaParams = makeLambdaParams(None, tparams).toMap
    val appliedParents = tpeBases(component).filterNot(isHKTOrPolyTypeOrResultTypeArtifact)
    val componentRef = makeRef(component)
    val parentEdges = appliedParents.map(parentTpe => componentRef -> resolveAppliedParentRef(parentTpe, lambdaParams))
    val refinementEdges = buildAppliedRefinementEdges(component, componentRef -> lambdaParams)
    parentEdges ::: refinementEdges
  }

  private[this] def resolveAppliedParentRef(
    parentTpe: Type,
    lambdaParams: Map[String, SymName.LambdaParamName]
  ): AbstractReference = {
    makeRefTop(parentTpe, terminalNames = lambdaParams, isLambdaOutput = lambdaParams.nonEmpty) match {
      case unapplied: Lambda =>
        if (unapplied.someArgumentsReferenced) unapplied else unapplied.output
      case applied: AppliedReference =>
        applied
    }
  }

  private[this] def buildAppliedRefinementEdges(
    component: Type,
    refAndParams: (AbstractReference, Map[String, SymName.LambdaParamName])
  ): List[(AbstractReference, AbstractReference)] = {
    val (componentRef, lambdaParams) = refAndParams
    componentRef match {
      case appliedSelf: AppliedReference =>
        val memberDecls = buildClassTypeMemberDecls(component, lambdaParams)
        if (memberDecls.nonEmpty) List(componentRef -> Refinement(appliedSelf, memberDecls)) else Nil
      case _ =>
        Nil
    }
  }

  private[this] def makeLambdaOnlyBases(mainTpe: Type, allReferenceComponents: Iterator[Type]): List[(AbstractReference, AbstractReference)] = {
    val unappliedBases = allReferenceComponents.flatMap(processLambdasReturningRefinements).toList
    logger.log(s"Computed lambda only bases for tpe=$mainTpe lambdaBases=${unappliedBases.toMultimap.niceList()}")
    unappliedBases
  }

  private[this] def processLambdasReturningRefinements(tpeRaw0: Type): List[(AbstractReference, AbstractReference)] = {
    val componentsOfPolyTypeResultType = UniRefinement.breakRefinement(tpeRaw0, squashHKTRefToPolyTypeResultType = true)

    IzAssert(
      assertion = {
        if (componentsOfPolyTypeResultType.maybeUnbrokenType.isEmpty) {
          !componentsOfPolyTypeResultType.intersectionComponents.exists(_.takesTypeArgs)
        } else {
          true
        }
      },
      clue = s"Unexpected intersection contains a PolyType: tpeRaw0 = $tpeRaw0"
    )

    componentsOfPolyTypeResultType
      .intersectionComponents.iterator.flatMap {
        component =>
          val componentAsPolyType = component.etaExpand
          val tparams = componentAsPolyType.typeParams
          if (tparams.isEmpty) Nil else makeLambda(componentAsPolyType, tparams)
      }.toList
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

    val baseParents = allBaseTypes.map {
      parentTpe =>
        val rawRef = makeRefTop(parentTpe, terminalNames = paramMap, isLambdaOutput = false)
        wrapLambdaIfReferenced(lambdaParamsUnpacked, rawRef)
    }
    val refinementParents = buildLambdaRefinementParents(componentPolyType, paramMap -> lambdaParamsUnpacked)
    baseParents ::: refinementParents
  }

  private[this] def wrapLambdaIfReferenced(
    lambdaParamsUnpacked: List[SymName.LambdaParamName],
    reference: AbstractReference
  ): AbstractReference = {
    reference match {
      case l: Lambda =>
        l
      case applied: AppliedReference =>
        val l = Lambda(lambdaParamsUnpacked, applied)
        if (l.someArgumentsReferenced) l else applied
    }
  }

  private[this] def buildLambdaRefinementParents(
    componentPolyType: Type,
    paramsCtx: (Map[String, SymName.LambdaParamName], List[SymName.LambdaParamName])
  ): List[AbstractReference] = {
    val (paramMap, lambdaParamsUnpacked) = paramsCtx
    val polyResult = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(componentPolyType)
    val memberDecls = buildClassTypeMemberDecls(polyResult, paramMap)
    if (memberDecls.isEmpty) {
      Nil
    } else {
      makeRefTop(polyResult, terminalNames = paramMap, isLambdaOutput = false) match {
        case appliedSelf: AppliedReference =>
          List(wrapLambdaIfReferenced(lambdaParamsUnpacked, Refinement(appliedSelf, memberDecls)))
        case _ =>
          Nil
      }
    }
  }

  private[this] def makeClassOnlyInheritanceDb(mainTpe: Type, allReferenceComponents: Iterator[Type]): Map[NameReference, Set[NameReference]] = {
    val baseclassReferences = allReferenceComponents
      .flatMap(UniRefinement.breakRefinement(_, squashHKTRefToPolyTypeResultType = true).intersectionComponents)
      .flatMap {
        component =>
          val prefix = makePrefixReference(component)
          val componentRef = makeNameReference(component, component.typeSymbol, Boundaries.Empty, prefix)
          val appliedBases = tpeBases(component).filterNot(isHKTOrPolyType)
          appliedBases.map(componentRef -> makeRef(_))
      }

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

  private[this] def tpeBases(t0: Type): List[Type] = {
    val tpe = Dealias.fullNormDealias(t0)
    val tpeSig = tpe.typeSymbol.typeSignature
    val upperBound = tpeSig.finalResultType match {
      case b: TypeBoundsApi if tpeSig.takesTypeArgs => List(b.hi)
      case _ => Nil
    }

    (upperBound.iterator ++ tpe.baseClasses.iterator.map(tpe.baseType))
      .filterNot(ignored)
      .filterNot(if (isSingletonType(tpe)) _ => false else _.typeSymbol.fullName == tpe.typeSymbol.fullName)
      .filterNot(_ =:= tpe)
      .toList
  }

  private[this] def makeRef(tpe: Type): AbstractReference = {
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
    makeRefImpl(RefContext(0, Set(tpe), terminalNames, Set.empty), TargetSpec(tpe, isLambdaOutput))
  }

  private[this] def makeRefImpl(ctx: RefContext, target: TargetSpec): AbstractReference = {
    val tpe0 = target.tpe
    tpe0 match {
      case l if target.isLambdaOutput =>
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

  private[this] def makeBoundaries(ctx: RefContext, t: Type): Boundaries = {
    val tOrTypeSymBounds = t match {
      case b: TypeBoundsApi => b
      case _ => t.typeSymbol.typeSignature
    }
    tOrTypeSymBounds match {
      case b: TypeBoundsApi if !isTrivialOrRecursiveBound(ctx, b) =>
        val lo = makeRefSub(ctx, b.lo, Map.empty)
        val hi = makeRefSub(ctx, b.hi, Map.empty)
        Boundaries.Defined(lo, hi)
      case _ =>
        Boundaries.Empty
    }
  }

  private[this] def isTrivialOrRecursiveBound(ctx: RefContext, b: TypeBoundsApi): Boolean = {
    val isUnbounded = b.lo =:= nothing && b.hi =:= any
    val isRecursive = ctx.nestedIn.contains(b.lo) || ctx.nestedIn.contains(b.hi)
    isUnbounded || isRecursive
  }

  private[this] def makeRefSub(
    ctx: RefContext,
    tpe: Type,
    stop: Map[String, SymName.LambdaParamName],
    knownWildcardsSub: Set[Symbol] = Set.empty
  ): AbstractReference = {
    val allWildcards = ctx.knownWildcards ++ knownWildcardsSub
    if (allWildcards.contains(tpe.typeSymbol)) {
      WildcardReference(makeBoundaries(ctx, tpe))
    } else {
      makeRefImpl(ctx.advance(tpe, stop, allWildcards), TargetSpec(tpe, isLambdaOutput = false))
    }
  }

  private[this] def unpackLambda(ctx: RefContext, t: TypeApi): AbstractReference = {
    val thisLevel = logger.sub(ctx.level)
    val polyType = t.etaExpand
    val polyTypeResult = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(polyType)

    val tparams = polyType.typeParams
    val nestingLevel = if (ctx.level > 0) Some(ctx.level) else None
    val lambdaParams = makeLambdaParams(nestingLevel, tparams)

    thisLevel.log(s"✴️ λ type $t has parameters $lambdaParams and result $polyTypeResult terminal names = ${ctx.terminalNames}")
    val reference = makeRefSub(ctx, polyTypeResult, lambdaParams.toMap)
    val out = Lambda(lambdaParams.map(_._2), reference)
    thisLevel.log(s"✳️ Restored λ $t => ${out.longNameWithPrefix}")
    out
  }

  private[this] def unpackProperTypeRefinement(ctx: RefContext, t0: Type): AppliedReference = {
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

  private[this] def unpackAsProperType(ctx: RefContext, tpeRaw: Type): AppliedNamedReference = {
    val thisLevel = logger.sub(ctx.level)
    val tpe = Dealias.fullNormDealias(tpeRaw)
    val prefix = makePrefixReference(tpe)
    val typeSymbol = tpe.typeSymbol
    val boundaries = makeBoundaries(ctx, tpe)

    val nameRef = ctx.terminalNames.get(typeSymbol.fullName) match {
      case Some(lambdaParameter) =>
        NameReference(lambdaParameter, boundaries, prefix)
      case None =>
        makeNameReference(tpe, typeSymbol, boundaries, prefix)
    }

    tpe.typeArgs match {
      case Nil =>
        nameRef
      case args =>
        val tparams = Dealias.fullNormDealias(tpeRaw).typeConstructor.typeParams
        val refParams = unpackTypeArgParams(ctx, tpeRaw -> args, tparams)
        val res = FullReference(nameRef.asName.ref, refParams, prefix)
        thisLevel.log(s"Assembled FullReference=$res from args=$args and tparams=$tparams")
        res
    }
  }

  private[this] def unpackTypeArgParams(
    ctx: RefContext,
    rawAndArgs: (Type, List[Type]),
    tparams: List[Symbol]
  ): List[TypeParam] = {
    val (tpeRaw, args) = rawAndArgs
    tpeRaw match {
      case t: ExistentialTypeApi =>
        val quantifiedParams = t.quantified.toSet
        t.underlying.typeArgs.zip(tparams).map {
          case (arg, param) =>
            val paramRef = if (quantifiedParams.contains(arg.typeSymbol) && !ctx.terminalNames.contains(arg.typeSymbol.fullName)) {
              WildcardReference(makeBoundaries(ctx, arg))
            } else {
              makeRefSub(ctx, arg, Map.empty, quantifiedParams)
            }
            TypeParam(paramRef, makeVariance(param.asType))
        }
      case _ =>
        args.zip(tparams).map {
          case (arg, param) =>
            TypeParam(makeRefSub(ctx, arg, Map.empty), makeVariance(param.asType))
        }
    }
  }

  private[this] def convertDecl(ctx: RefContext, decl: Symbol): CollectionCompat.IterableOnce[RefinementDecl] = {
    if (decl.isMethod) {
      convertMethodDecl(ctx, decl.asMethod)
    } else if (decl.isType) {
      val tpe = UniRefinement.typeOfTypeMember(decl)
      val declName = decl.name.decodedName.toString
      val ref = makeRefSub(ctx, tpe, Map.empty) match {
        case n @ NameReference(SymTypeName("<none>"), _, _) => n.copy(ref = SymTypeName(declName))
        case other => other
      }
      Some(RefinementDecl.TypeMember(declName, ref))
    } else {
      None
    }
  }

  private[this] def convertMethodDecl(ctx: RefContext, declMethod: MethodSymbol): List[RefinementDecl.Signature] = {
    val returnTpe = declMethod.returnType
    val paramLists0 = declMethod.paramLists.map(_.map {
      param =>
        makeRefSub(ctx, UniRefinement.typeOfParam(param), Map.empty).asInstanceOf[AppliedReference]
    })
    val paramLists = if (paramLists0.nonEmpty) paramLists0 else List(Nil)
    val returnRef = makeRefSub(ctx, returnTpe, Map.empty).asInstanceOf[AppliedReference]

    paramLists.map {
      parameterList =>
        RefinementDecl.Signature(declMethod.name.decodedName.toString, parameterList, returnRef)
    }
  }

  private[this] def makeNameReference(originalType: Type, typeSymbol: Symbol, boundaries: Boundaries, prefix: Option[AppliedReference]): NameReference = {
    originalType match {
      case c: ConstantTypeApi =>
        NameReference(SymLiteral(c.value.value), boundaries, None)

      case s: SingleTypeApi if s.sym != NoSymbol =>
        s.sym.info.finalResultType match {
          case c: ConstantTypeApi =>
            NameReference(SymLiteral(c.value.value), boundaries, None)

          case _ =>
            val (sym, _) = Dealias.dealiasSingletons(s.termSymbol, originalType)
            val resultType = Dealias.fullNormDealias(sym.typeSignatureIn(s.pre).finalResultType)
            val newPrefix = if (hasSingletonType(resultType.typeSymbol)) makePrefixReference(resultType) else prefix
            NameReference(makeSymName(sym), boundaries, newPrefix)
        }

      case _ =>
        NameReference(makeSymName(typeSymbol), boundaries, prefix)
    }
  }

  private[this] def makePrefixReference(originalType: Type): Option[AppliedReference] = {
    extractPrefix(originalType).flatMap(unpackPrefix(originalType, _))
  }

  @tailrec private[this] def extractPrefix(t0: Type): Option[Type] = {
    t0 match {
      case t: TypeRefApi => Some(t.pre).filterNot(_ == NoPrefix)
      case t: SingleTypeApi => Some(t.pre).filterNot(_ == NoPrefix)
      case t: ExistentialTypeApi => extractPrefix(t.underlying)
      case t: ThisTypeApi => extractPrefix(if (t.sym.isType) t.sym.asType.toType else t.sym.asTerm.typeSignature)
      case _ => None
    }
  }

  private[this] def unpackPrefix(originalType: Type, pre: Type): Option[AppliedReference] = {
    pre match {
      case i if i.typeSymbol.isPackage || i == NoPrefix =>
        None
      case k: ThisTypeApi =>
        unpackThisPrefix(originalType, k)
      case k if k.termSymbol != NoSymbol =>
        handleSingletonType(k)
      case o =>
        fromPrefixRef(originalType, o)
    }
  }

  private[this] def unpackThisPrefix(originalType: Type, k: ThisTypeApi): Option[AppliedReference] = {
    k.sym.asType.toType match {
      case UniRefinement(_, _) =>
        None
      case _ =>
        val ownerSym = if (originalType.termSymbol != NoSymbol) originalType.termSymbol.owner else originalType.typeSymbol.owner
        fromPrefixRef(originalType, ownerSym.asType.toType)
    }
  }

  private[this] def handleSingletonType(initType: Type): Some[NameReference] = {
    val termSym = initType.termSymbol
    val (dealiasedSym, rawTpe0) = if (!termSym.isModule) {
      Dealias.dealiasSingletons(termSym, initType)
    } else {
      (termSym, initType)
    }
    val dealiasedTpe = Dealias.fullNormDealias(rawTpe0)
    val name = makeSymName(dealiasedSym)
    val prePrefix = makePrefixReference(dealiasedTpe)

    Some(NameReference(name, Boundaries.Empty, prePrefix))
  }

  private[this] def fromPrefixRef(originalType: Type, o: Type): Option[AppliedReference] = {
    makeRef(o) match {
      case a: AppliedReference =>
        Some(a)
      case other =>
        throw new IllegalStateException(s"Cannot extract prefix from $originalType: expected applied reference, but got $other")
    }
  }

}
