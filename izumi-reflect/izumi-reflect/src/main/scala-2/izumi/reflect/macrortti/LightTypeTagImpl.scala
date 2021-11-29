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

import izumi.reflect.internal.fundamentals.collections.IzCollections.*
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.strings.IzString.*
import izumi.reflect.macrortti.LightTypeTagImpl.{Broken, globalCache}
import izumi.reflect.macrortti.LightTypeTagRef.RefinementDecl.TypeMember
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymLiteral, SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef.*
import izumi.reflect.{DebugProperties, ReflectionUtil}
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.language.reflectiveCalls
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

  private[reflect] sealed trait Broken[T, S] {
    def intersectionComponents: Set[T]
    def decls: Set[S]
    def maybeUnbrokenType: Option[T]
  }

  private[reflect] object Broken {

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

final class LightTypeTagImpl[U <: Universe with Singleton](val u: U, withCache: Boolean, logger: TrivialLogger) {

  import u._

  @inline private[this] final val any = definitions.AnyTpe
  @inline private[this] final val obj = definitions.ObjectTpe
  @inline private[this] final val nothing = definitions.NothingTpe
  @inline private[this] final val ignored = Set(any, obj, nothing)

  def makeFullTagImpl(tpe0: Type): LightTypeTag = {
    val tpe = Dealias.fullNormDealias(tpe0)

    val lttRef = makeRef(tpe)

    val allReferenceComponents = allTypeReferences(tpe)

    val fullDb = {
      val stableBases = makeAppliedBases(tpe, allReferenceComponents)
      val basesAsLambdas = makeLambdaOnlyBases(tpe, allReferenceComponents)
      val allBases = Seq(basesAsLambdas, stableBases)
      allBases.iterator.flatten.toMultimap.filterNot(_._2.isEmpty)
    }

    val unappliedDb = {
      makeUnappliedInheritanceDb(allReferenceComponents)
    }

    LightTypeTag(lttRef, fullDb, unappliedDb)
  }

  private def allTypeReferences(tpe0: Type): Set[Type] = {

    def extractComponents(tpeRaw0: Type, inh: mutable.HashSet[Type]): Unit = {
      val breakResult = UniRefinement.breakRefinement(tpeRaw0, squashHKTRefToPolyTypeResultType = false)
      val current = breakResult.maybeUnbrokenType
      inh ++= current

      val intersectionWithPreservedLambdas = breakResult.intersectionComponents
      val refinementDeclMembers = breakResult.decls.iterator.flatMap {
        sym =>
          if (sym.isMethod) {
            val m = sym.asMethod
            m.returnType :: m.paramLists.iterator.flatten.map(UniRefinement.typeOfParam).toList
          } else if (sym.isType) {
            List(UniRefinement.typeOfTypeMember(sym))
          } else Nil
      }
      val intersectionExpansionsArgsBounds = intersectionWithPreservedLambdas.iterator.flatMap(collectEtaExpansionArgsBounds)

      val nextToInspect = (intersectionExpansionsArgsBounds.iterator ++ intersectionWithPreservedLambdas.iterator ++ refinementDeclMembers)
        .to(mutable.HashSet)
        .diff(inh)

      nextToInspect.foreach(t => if (!inh(t) && !ignored(t)) extractComponents(t, inh))
    }

    def collectEtaExpansionArgsBounds(tpeUnexpanded0: Type): Iterator[Type] = {
      // unexpanded: Either
      val tpeUnexpanded = Dealias.fullNormDealias(tpeUnexpanded0)
      // polyType: [L,R]Either[L,R]
      // polyTypeResult: Either[L,R] where L,R are trash symbols
      val tpePolyTypeResultType = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(tpeUnexpanded)

      logger.log(
        s"Got tpeUnexpanded=$tpeUnexpanded class=${tpeUnexpanded.getClass} args=${tpeUnexpanded.typeArgs} params=${tpeUnexpanded.typeParams}"
      )

      // we need to use tpe.etaExpand but 2.13 has a bug: https://github.com/scala/bug/issues/11673#
      // tpe.etaExpand.resultType.dealias.typeArgs.flatMap(_.dealias.resultType.typeSymbol.typeSignature match {
      val tparamTypeBoundsAndTypeArgs = tpePolyTypeResultType.typeArgs.flatMap {
        targ0 =>
          val targ = Dealias.fullNormDealias(targ0)
          val targSym = targ.typeSymbol
          targSym.typeSignature match {
            case t: TypeBoundsApi =>
              Seq(t.hi, t.lo).filterNot(ignored)
            case _ =>
              if (!targSym.isParameter) Seq(targ0) else Seq.empty
          }
      }

      logger.log(
        s"Got tpePolyTypeResultType=$tpePolyTypeResultType class=${tpePolyTypeResultType.getClass} args=${tpePolyTypeResultType.typeArgs} params=${tpePolyTypeResultType.typeParams} tparamTypeBounds=$tparamTypeBoundsAndTypeArgs"
      )
      val tpePolyType = tpeUnexpanded.etaExpand
      logger.log(
        s"Got tpePolyType=$tpePolyType class=${tpePolyType.getClass} args=${tpePolyType.typeArgs} params=${tpePolyType.typeParams}"
      )

      Iterator.single(tpeUnexpanded) ++
      tpeUnexpanded.typeArgs.iterator ++
//      Iterator.single(tpePolyTypeResultType) ++ // trash
//      tpePolyTypeResultType.typeArgs.iterator ++ // redundant, included below
      tparamTypeBoundsAndTypeArgs.iterator
    }

    val inh = mutable.HashSet[Type]()
    extractComponents(tpe0, inh)
    logger.log(s"Extracted type references for tpe=$tpe0 inh=${inh.iterator.map(t => (t, t.getClass)).toMap.niceList()}")

    inh.toSet
  }

  private def makeRef(tpe: Type): AbstractReference = {
    if (withCache) {
      globalCache.synchronized(globalCache.get(tpe)) match {
        case null =>
          val ref = makeRef(tpe, terminalNames = Map.empty)
          globalCache.synchronized(globalCache.put(tpe, ref))
          ref
        case ref =>
          ref
      }
    } else {
      makeRef(tpe, terminalNames = Map.empty)
    }
  }

  private def makeRef(tpe: Type, terminalNames: Map[String, LambdaParameter], isLambdaOutput: Boolean = false): AbstractReference = {
    val tuple = makeRef0(0)(tpe, Set(tpe), terminalNames, isLambdaOutput, mutable.HashSet.empty)
    if (tuple._2.nonEmpty) logger.log(s"Gathered inh=${tuple._2}")
    tuple._1
  }

  private def makeRef0(
    level: Int
  )(tpe0: Type,
    path: Set[Type],
    terminalNames: Map[String, LambdaParameter],
    isLambdaOutput: Boolean,
    inh: mutable.HashSet[Type]
  ): (AbstractReference, inh.type) = {
    val thisLevel = logger.sub(level)

    def unpackLambda(t: TypeApi): AbstractReference = {
      val polyType = t.etaExpand
      val polyTypeResult = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(polyType)

      val tparams = polyType.typeParams
      val nestingLevel = if (level > 0) Some(level) else None
      val lambdaParams = makeLambdaParams(nestingLevel, tparams)

      thisLevel.log(s"✴️ λ type $t has parameters $lambdaParams, terminal names = $terminalNames")
      val reference = makeRefSub(polyTypeResult, lambdaParams.toMap)
      val out = Lambda(lambdaParams.map(_._2), reference)
      if (!out.allArgumentsReferenced) {
        val kvParams = lambdaParams.map { case (k, v) => s"$v = $k" }
        thisLevel.log(
          s"⚠️ unused 𝝺 args! type $t => $out, someReferenced: ${out.someArgumentsReferenced} context: $terminalNames, 𝝺 params: $kvParams, 𝝺 result: $polyTypeResult => $reference, referenced: ${out.referenced} "
        )
      }

      thisLevel.log(s"✳️ Restored λ $t => ${out.longName}")
      out
    }

    def unpackNonLambdaRefinement(t0: Type, rules: Map[String, LambdaParameter]): AppliedReference = {
      require(!isHKTOrPolyType(Dealias.fullNormDealias(t0)))
      UniRefinement.breakRefinement(t0, squashHKTRefToPolyTypeResultType = false) match {
        case Broken.Compound(tpes, decls) =>
          val parts = tpes.map(unpackAsProperType(_, rules): AppliedReference)
          val intersection = LightTypeTagRef.maybeIntersection(parts)
          if (decls.nonEmpty) {
            Refinement(intersection, convertDecls(decls.toList, rules).to(SortedSet))
          } else {
            intersection
          }

        case Broken.Single(t) =>
          t0 match {
            case p if isHKTOrPolyType(p) =>
              // we intentionally ignore breakRefinement result here, it breaks lambdas (with squashHKTRefToPolyTypeResultType = true)
              throw new RuntimeException(p.getClass.->(p.etaExpand.getClass -> p.etaExpand.takesTypeArgs).toString)
              unpackAsProperType(p, rules)
            case _ =>
              unpackAsProperType(t, rules)
          }
      }
    }

    def unpackAsProperType(tpeRaw: Type, rules: Map[String, LambdaParameter]): AppliedNamedReference = {
      val tpe = Dealias.fullNormDealias(tpeRaw)
      val prefix = getPrefix(tpe)
      val typeSymbol = tpe.typeSymbol
      val boundaries = makeBoundaries(tpe)
      val nameref = rules.get(typeSymbol.fullName) match {
        case Some(lambdaParameter) =>
          // this is a previously encountered type variable
          NameReference(SymTypeName(lambdaParameter.name), boundaries, prefix)

        case None =>
          makeNameReference(tpe, typeSymbol, boundaries, prefix)
      }

      tpe.typeArgs match {
        case Nil => nameref

        case args =>
          val tparams = tpeRaw.dealias.typeConstructor.typeParams
          val refParams = args.zip(tparams).map {
            case (arg, param) =>
              TypeParam(makeRefSub(arg), getVariance(param.asType))
          }
          val res = FullReference(nameref.ref.name, refParams, prefix)
          thisLevel.log(s"Assembled FullReference=$res from args=$args and tparams=$tparams")
          res
      }
    }

    def convertDecls(decls: List[SymbolApi], rules: Map[String, LambdaParameter]): List[RefinementDecl] = {
      decls.flatMap {
        decl =>
          if (decl.isMethod) {
            val m = decl.asMethod
            val ret = m.returnType

            val params = m.paramLists.map {
              paramlist =>
                paramlist.map {
                  p =>
                    val pt = UniRefinement.typeOfParam(p)
                    makeRefSub(pt, rules).asInstanceOf[AppliedReference]
                }
            }

            val inputs = if (params.nonEmpty) {
              params
            } else {
              Seq(Seq.empty)
            }

            inputs.map {
              pl =>
                RefinementDecl.Signature(m.name.decodedName.toString, pl.toList, makeRefSub(ret, rules).asInstanceOf[AppliedReference])
            }
          } else if (decl.isType) {
            val tpe = UniRefinement.typeOfTypeMember(decl)
            val ref = makeRefSub(tpe, rules)
            Seq(TypeMember(decl.name.decodedName.toString, ref))
          } else {
            None
          }
      }
    }

    def makeBoundaries(t: Type): Boundaries = {
      t.typeSymbol.typeSignature match {
        case b: TypeBoundsApi =>
          if ((b.lo =:= nothing && b.hi =:= any) || (path.contains(b.lo) || path.contains(b.hi))) {
            Boundaries.Empty
          } else {
            Boundaries.Defined(makeRefSub(b.lo), makeRefSub(b.hi))
          }
        case _ =>
          Boundaries.Empty
      }
    }

    def makeRefSub(tpe: Type, stop: Map[String, LambdaParameter] = Map.empty): AbstractReference = {
      this.makeRef0(level + 1)(tpe, path + tpe, terminalNames ++ stop, isLambdaOutput = false, inh)._1
    }

    val out = tpe0 match {
      case l if isLambdaOutput => // this is required for handling SwapF2, etc.
        require(!isHKTOrPolyType(l), l -> l.getClass)
        Lambda(terminalNames.values.toList, unpackAsProperType(l, terminalNames))

      case l: PolyTypeApi =>
        unpackLambda(l)

      case l if l.takesTypeArgs =>
        if (terminalNames.contains(l.typeSymbol.fullName)) {
          unpackAsProperType(l, terminalNames)
        } else {
          unpackLambda(l)
        }

      case c =>
        unpackNonLambdaRefinement(c, terminalNames)
    }
    (out, inh)
  }

  private def makeAppliedBases(mainTpe: Type, allReferenceComponents: Set[Type]): Set[(AbstractReference, AbstractReference)] = {

    val appliedBases = allReferenceComponents
      .filterNot(isHKTOrPolyType) // remove PolyTypes, only process applied types in this inspection
      .flatMap {
        component =>
          val appliedParents = tpeBases(component).filterNot(isHKTOrPolyType)
          val tparams = component.etaExpand.typeParams
          val lambdaParams = makeLambdaParams(None, tparams).toMap

          val componentRef = makeRef(component)

          appliedParents.map {
            parentTpe =>
              val parentRef = makeRef(parentTpe, terminalNames = lambdaParams, isLambdaOutput = lambdaParams.nonEmpty) match {
                case unapplied: Lambda =>
                  if (unapplied.someArgumentsReferenced) {
                    unapplied
                  } else {
//                    require(
//                      unapplied.someArgumentsReferenced,
//                      s"Not all arguments referenced in l=$unapplied, base=$parentTpe(expand:${parentTpe.etaExpand}), tparams=$tparams(tpe.expand:${mainTpe.etaExpand})"
//                    )
                    unapplied.output
                  }
                case applied: AppliedReference =>
                  applied
              }
              (componentRef, parentRef)
          }
      }
    logger.log(s"Computed applied bases for tpe=$mainTpe appliedBases=${appliedBases.toMultimap.niceList()}")
    appliedBases
  }

  private def makeLambdaOnlyBases(mainTpe: Type, allReferenceComponents: Set[Type]): Set[(AbstractReference, AbstractReference)] = {

    def processLambdasReturningRefinements(tpeRaw0: Type): Seq[(AbstractReference, AbstractReference)] = {
      val componentsOfPolyTypeResultType = UniRefinement.breakRefinement(tpeRaw0, squashHKTRefToPolyTypeResultType = true)

      componentsOfPolyTypeResultType.intersectionComponents.toSeq.flatMap {
        component =>
          val componentAsPolyType = component.etaExpand
          val tparams = component.etaExpand.typeParams

          if (tparams.isEmpty) {
            Seq.empty
          } else {
            require(
              if (componentsOfPolyTypeResultType.maybeUnbrokenType.nonEmpty) {
                componentsOfPolyTypeResultType.intersectionComponents.exists(_.etaExpand.typeParams.nonEmpty)
              } else true,
              message = componentsOfPolyTypeResultType.intersectionComponents.niceList(prefix = "*") ->
                componentsOfPolyTypeResultType.intersectionComponents.map(_.etaExpand).niceList(prefix = "+") ->
                componentsOfPolyTypeResultType.intersectionComponents.map(_.etaExpand.typeParams).niceList(prefix = "-")
            )

            val lambdaParams = makeLambdaParams(None, tparams)
            val maybeComponentLambdaRef = makeRef(componentAsPolyType)
            require(maybeComponentLambdaRef.isInstanceOf[LightTypeTagRef.Lambda])
            makeParentLambdas(componentAsPolyType, lambdaParams)
              .map(maybeComponentLambdaRef -> _)
          }
      }
    }

    def makeParentLambdas(componentPolyType: Type, lambdaParams: List[(String, LambdaParameter)]): Seq[AbstractReference] = {
      val allBaseTypes = tpeBases(componentPolyType)

      val paramMap = lambdaParams.toMap

      allBaseTypes.map {
        parentTpe =>
          val reference = makeRef(parentTpe, terminalNames = paramMap)
          reference match {
            case l: Lambda =>
              l
            case applied: AppliedReference =>
              val l = Lambda(lambdaParams.map(_._2), applied)
//              Some(l).filter(_.allArgumentsReferenced) // do not include non-lambda parents such as Product into lambda's inheritance tree
              // include ALL bases for lambdas (this should be more correct since lambda is a template for a full parameterized db after combine)
              if (l.someArgumentsReferenced) l else applied
          }
      }
    }

    val unappliedBases = {
      allReferenceComponents.flatMap(processLambdasReturningRefinements)
    }
    logger.log(s"Computed unapplied bases for tpe=$mainTpe unappliedBases=${unappliedBases.niceList()}")
    unappliedBases
  }

  private def makeUnappliedInheritanceDb(allReferenceComponents: Set[Type]): Map[NameReference, Set[NameReference]] = {
    val baseclassReferences = allReferenceComponents
      .iterator
      .flatMap {
        // squash all type lambdas and get the intersection of their results
        // because we don't care about type parameters at all in this inspection
        UniRefinement.breakRefinement(_, squashHKTRefToPolyTypeResultType = true).intersectionComponents
      }
      .flatMap {
        component =>
          val prefix = getPrefix(component)
          val componentRef = makeNameReference(component, component.typeSymbol, Boundaries.Empty, prefix)

          // this doesn't seem to ever happen
          val innerSymbolName = component match {
            case a: TypeRefApi =>
              val innerSymName = symName(a.sym)
              if (innerSymName != componentRef.ref) {
                require(false, innerSymName -> componentRef)
                val innerRef = NameReference(innerSymName, Boundaries.Empty, prefix)
                Seq(innerRef -> componentRef)
              } else {
                Seq.empty
              }
            case _ =>
              Seq.empty
          }

          val appliedBases = tpeBases(component).filterNot(isHKTOrPolyType)

          innerSymbolName ++ appliedBases.map(componentRef -> makeRef(_))
      }

    val unparameterizedInheritanceData = baseclassReferences
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents
            .collect {
              case r: AppliedNamedReference =>
                r.asName
            }
            .filterNot {
              parent =>
                require(parent != t, parent -> t)
                parent == t
            }
      }
      .filterNot(_._2.isEmpty)

    unparameterizedInheritanceData
  }

  private def tpeBases(t0: Type): Seq[Type] = {
//    val tpef = Dealias.fullNormDealiasResultType(t0, squashHKTRefToPolyTypeResultType = false)
    // no PolyTypes passed to here
    val tpef = Dealias.fullNormDealias(t0)
    val higherBases = tpef.baseClasses
    val onlyParameterizedBases = {
      higherBases
        .filterNot {
          s =>
            !s.isType || {
              val btype = s.asType.toType
              ignored.exists(_ =:= btype) || btype =:= tpef
            }
        }
        .map(s => tpef.baseType(s))
    }
    val allbases = onlyParameterizedBases.filterNot(_ =:= tpef)

    val newBases = {

      //    val tpef = Dealias.fullNormDealiasResultType(t0, squashHKTRefToPolyTypeResultType = false)
      // no PolyTypes passed to here [actually we should preserve polytypes]
      val tpe = Dealias.fullNormDealias(t0)
      tpe
        .baseClasses
        .iterator
        .map(tpe.baseType)
        .filterNot(ignored)
        .filterNot(if (isSingletonType(tpe)) _ => false else _.typeSymbol.fullName == tpe.typeSymbol.fullName)
        .filterNot(_ =:= tpe)
        .toList
    }

    val oldremove = allbases.toSet.diff(newBases.toSet)
    val n = newBases.toSet.diff(allbases.toSet)
    if (oldremove.nonEmpty) {
      require(!isSingletonType(tpef), s"blahold removed $tpef $oldremove $tpef ${isSingletonType(tpef)} ${tpef.getClass}")
      logger.log(s"blahold removed $tpef $oldremove")
    }
    if (n.nonEmpty) logger.log(s"blahnew added $tpef $n")
    newBases
  }

  private def makeLambdaParams(ctxIdx: Option[Int], tparams: List[Symbol]): List[(String, LambdaParameter)] = {
    tparams.zipWithIndex.map {
      case (tparamSym, idx) =>
        val fullName = tparamSym.fullName
        val idxStr = ctxIdx match {
          case Some(ctx) =>
            s"$ctx:$idx"
          case None =>
            idx.toString
        }
        fullName -> LambdaParameter(idxStr)
    }
  }

  private object UniRefinement {

    def unapply(tpe: Type): Option[(List[Type], List[SymbolApi])] = {
      (tpe: AnyRef) match {
        case x: scala.reflect.internal.Types#RefinementTypeRef =>
          Some((x.parents.asInstanceOf[List[Type]], x.decls.toList.asInstanceOf[List[SymbolApi]]))
        case r: RefinedTypeApi @unchecked =>
          Some((r.parents, r.decls.toList))
        case _ =>
          None
      }
    }

    def breakRefinement(t0: Type, squashHKTRefToPolyTypeResultType: Boolean): Broken[Type, SymbolApi] = {
      breakRefinement0(t0, squashHKTRefToPolyTypeResultType) match {
        case (t, d) if d.isEmpty && t.size == 1 =>
          Broken.Single(t.head)
        case (t, d) =>
          logger.log(s"Found compound type parents=$t decls=$d")
          Broken.Compound(t, d)
      }
    }

    private def breakRefinement0(t0: Type, squashHKTRefToPolyTypeResultType: Boolean): (Set[Type], Set[SymbolApi]) = {
      val normalized = if (squashHKTRefToPolyTypeResultType) {
        Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(t0)
      } else {
        Dealias.fullNormDealias(t0)
      }
      normalized match {
        case UniRefinement(parents, decls) =>
          val parts = parents.map(breakRefinement0(_, squashHKTRefToPolyTypeResultType))
          val types = parts.flatMap(_._1)
          val partsDecls = parts.flatMap(_._2)
          (types.toSet, (decls ++ partsDecls).toSet)
        case t =>
          (Set(t), Set.empty)
      }
    }

    def typeOfParam(p: u.Symbol): Type = {
      p.typeSignature
    }

    def typeOfTypeMember(decl: u.SymbolApi): Type = {
      if (decl.isAbstract) {
        decl.asType.toType
      } else {
        decl.typeSignature
      }
    }

  }

  private object Dealias {

    def fullNormDealiasSquashHKTToPolyTypeResultType(t0: Type): Type = {
      var prev = null: Type
      val t1 = fullNormDealias(t0)
      var cur = if (t1.takesTypeArgs) t1.etaExpand else t1
      while (cur ne prev) {
        prev = cur
        cur = norm(prev).dealias.resultType
      }
      cur
    }

    def fullNormDealias(t0: Type): Type = {
      var prev = null: Type
      var cur = t0
      while (cur ne prev) {
        prev = cur
        cur = norm(prev).dealias
      }
      cur
    }

    @tailrec
    def dealiasSingletons(termSymbol: Symbol): Symbol = {
      val resultTerm = termSymbol.typeSignature.finalResultType.termSymbol
      if (hasSingletonType(resultTerm)) {
        dealiasSingletons(resultTerm)
      } else {
        termSymbol
      }
    }

    @inline def norm(x: Type): Type = {
      ReflectionUtil.norm(u: u.type, logger)(x)
    }

  }

  private def getPrefix(tpe: Type): Option[AppliedReference] = {

    def fromRef(o: Type): Option[AppliedReference] = {
      makeRef(o) match {
        case a: AppliedReference =>
          Some(a)
        case o =>
          throw new IllegalStateException(s"Cannot extract prefix from $tpe: expected applied reference, but got $o")
      }
    }

    def unpackPrefix(pre: Type): Option[AppliedReference] = {
      pre match {
        case i if i.typeSymbol.isPackage =>
          None
        case k if k == NoPrefix =>
          None
        case k: ThisTypeApi =>
          k.sym.asType.toType match {
            // This case matches UniRefinement.unapply#it.RefinementTypeRef case
            case UniRefinement(_, _) =>
              None
            case _ =>
              if (tpe.termSymbol != NoSymbol) {
                fromRef(tpe.termSymbol.owner.asType.toType)
              } else {
                fromRef(tpe.typeSymbol.owner.asType.toType)
              }
          }
        case k if k.termSymbol != NoSymbol =>
          val finalSymbol = Dealias.dealiasSingletons(k.termSymbol)
          val name = symName(finalSymbol)
          val prePrefix = getPrefix(finalSymbol.typeSignature.finalResultType)
          Some(NameReference(name, Boundaries.Empty, prePrefix))
        case o =>
          fromRef(o)
      }
    }

    @tailrec def getPre(t0: Type): Option[Type] = {
      t0 match {
        case t: TypeRefApi => Some(t.pre).filterNot(_ == NoPrefix)
        case t: SingleTypeApi => Some(t.pre).filterNot(_ == NoPrefix)
        case t: ExistentialTypeApi => getPre(t.underlying)
        case _ => None
      }
    }

    val prefix = getPre(tpe)
    val unpacked = prefix.flatMap(unpackPrefix)
    unpacked
  }

  private def makeNameReference(originalType: Type, typeSymbol: Symbol, boundaries: Boundaries, prefix: Option[AppliedReference]): NameReference = {
    originalType match {
      case c: ConstantTypeApi =>
        NameReference(SymLiteral(c.value.value), boundaries, prefix)

      case s: SingleTypeApi if s.sym != NoSymbol =>
        val sym = Dealias.dealiasSingletons(s.termSymbol)
        val resultType = sym.typeSignatureIn(s.pre).finalResultType
        val newPrefix = if (hasSingletonType(resultType.typeSymbol)) getPrefix(resultType) else prefix
        NameReference(symName(sym), boundaries, newPrefix)

      case _ =>
        NameReference(symName(typeSymbol), boundaries, prefix)
    }
  }

  private def symName(sym: Symbol): SymName = {
    val o = sym.owner
    val base = if (o.asInstanceOf[{ def hasMeaninglessName: Boolean }].hasMeaninglessName) {
      sym.name.decodedName.toString
    } else {
      sym.fullName
    }

    if (hasSingletonType(sym)) {
      SymTermName(base)
    } else {
      SymTypeName(base)
    }
  }

  private def isHKTOrPolyType(tpe: Type): Boolean = {
    tpe.takesTypeArgs || tpe.isInstanceOf[PolyTypeApi]
  }

  private def hasSingletonType(sym: Symbol): Boolean = {
    sym.isTerm || sym.isModuleClass || isSingletonType(sym.typeSignature)
  }

  @inline private def isSingletonType(tpe: Type): Boolean = {
    tpe.isInstanceOf[SingletonTypeApi] && !tpe.isInstanceOf[ThisTypeApi]
  }

  private def getVariance(tpes: TypeSymbol): Variance = {
    if (tpes.isCovariant) {
      Variance.Covariant
    } else if (tpes.isContravariant) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }
}
