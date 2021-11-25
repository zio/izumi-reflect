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
import izumi.reflect.DebugProperties

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
      val logger = TrivialLogger.make[this.type]()
      new LightTypeTagImpl[u.type](u, withCache = runtimeCacheEnabled, logger).makeFullTagImpl(typeTag)
    }
  }

  private[this] object ReflectionLock

  private[reflect] sealed trait Broken[T, S] {
    def intersectionComponents: Set[T]
    def decls: Set[S]
  }

  private[reflect] object Broken {

    final case class Single[T, S](t: T) extends Broken[T, S] {
      override def intersectionComponents: Set[T] = Set(t)
      override def decls: Set[S] = Set.empty
    }

    final case class Compound[T, S](intersectionComponents: Set[T], decls: Set[S]) extends Broken[T, S]

  }

}

final class LightTypeTagImpl[U <: Universe with Singleton](val u: U, withCache: Boolean, logger: TrivialLogger) {

  import u._

  @inline private[this] final val any = definitions.AnyTpe
  @inline private[this] final val obj = definitions.ObjectTpe
  @inline private[this] final val nothing = definitions.NothingTpe
  @inline private[this] final val ignored = Set(any, obj, nothing)

  @inline private[this] final val it = u.asInstanceOf[scala.reflect.internal.Types]

  def makeFullTagImpl(tpe0: Type): LightTypeTag = {
    val tpe = fullNormDealias(tpe0)
    val lttRef = makeRef(tpe)

    val allReferenceComponents: Set[Type] = {
//      UniRefinement.breakRefinement(tpe, destroyTypeLambdas = false).toSet ++

      allTypeReferences(tpe)
//      UniRefinement
//        .breakRefinement(tpe, destroyTypeLambdas = false).intersectionComponents.flatMap(
//          tpe01 => {
//            val value = allTypeReferences(tpe01)
//            value ++ value.flatMap(UniRefinement.breakRefinement(_, destroyTypeLambdas = true).intersectionComponents)
//          }
//        ) // ++ Set(tpe)

//      Set(tpe) ++
//      allTypeReferences(tpe).flatMap(UniRefinement.breakRefinement(_, destroyTypeLambdas = true).toSet)
    }

    logger.log(s"Got baseComponents=${allReferenceComponents.map(t => t -> t.getClass)}")

    val stableBases = makeStableBases(tpe, allReferenceComponents)
    val basesAsLambdas = allReferenceComponents.flatMap(makeBaseClasses)
    val allBases = Seq(
      basesAsLambdas,
      stableBases
    )
    val fullDb = allBases
      .flatten
      .toMultimap
      .filterNot(_._2.isEmpty)

    val unappliedDb = makeUnappliedInheritanceDb(allReferenceComponents)

    LightTypeTag(lttRef, fullDb, unappliedDb)
  }

  private def allTypeReferences(tpe0: Type): Set[Type] = {

    def extractTypeArgs(tpe0: Type, inh: mutable.HashSet[Type], destroyTypeLambdas: Boolean): Unit = {
      val tpeRaw = fullNormDealias(tpe0) // Either

      val tpeRefinement = UniRefinement.breakRefinement(tpeRaw, destroyTypeLambdas = true)
      val etaExpandedIntersections = tpeRefinement.intersectionComponents
      val refinementDeclMembers = tpeRefinement.decls.iterator.flatMap {
        sym =>
          if (sym.isMethod) {
            val m = sym.asMethod
            m.returnType :: m.paramLists.iterator.flatten.map(UniRefinement.typeOfParam).toList
          } else if (sym.isType) {
            List(UniRefinement.typeOfTypeMember(sym))
          } else Nil
      }

      val tpeLambda = fullNormDealiasResultType(tpeRaw, destroyTypeLambdas = false) // [L,R]Either[L,R]
      val current = Seq(tpeRaw, tpeLambda)
      inh ++= current

      logger.log(s"Type args tpe=$tpeRaw class=${tpeRaw.getClass} args=${tpeRaw.typeArgs}")
      logger.log(s"Type args normedTpe=$tpeLambda class=${tpeLambda.getClass} args=${tpeLambda.typeArgs}")

      // we need to use tpe.etaExpand but 2.13 has a bug: https://github.com/scala/bug/issues/11673#
      // tpe.etaExpand.resultType.dealias.typeArgs.flatMap(_.dealias.resultType.typeSymbol.typeSignature match {
      val tparamTypeBounds = tpeLambda.typeArgs.flatMap {
        t0 =>
          norm(fullNormDealiasResultType(t0, destroyTypeLambdas = false).typeSymbol.typeSignature) match {
            case t: TypeBoundsApi =>
              Seq(t.hi, t.lo)
            case _ =>
              Seq.empty
          }
      }
      val nextTypeArgs = tpeRaw.typeArgs ++ tpeLambda.typeArgs ++ tparamTypeBounds
      (nextTypeArgs.iterator ++ etaExpandedIntersections.iterator ++ refinementDeclMembers)
        .to(mutable.HashSet)
        .diff(inh)
        .foreach(extractTypeArgs(_, inh, destroyTypeLambdas = true)) // only first iteration should preserve lambdas
    }

    val inh = mutable.HashSet[Type]()
    extractTypeArgs(tpe0, inh, destroyTypeLambdas = false)

    logger.log(s"Extracted type references: $inh")

    inh.toSet
  }

  private def makeUnappliedInheritanceDb(allReferenceComponents: Set[Type]): Map[NameReference, Set[NameReference]] = {
    val baseclassReferences = allReferenceComponents.flatMap {
      i =>
        val (srcname, targetRef) = {
          val tpef = norm(i.dealias.resultType)
          val prefix = getPrefix(tpef)
          val targetRef = makeNameReference(i, tpef.typeSymbol, Boundaries.Empty, prefix)

          val srcname = i match {
            case a: TypeRefApi =>
              val srcname = symName(a.sym)
              if (srcname != targetRef.ref) {
                Seq((NameReference(srcname, Boundaries.Empty, prefix), targetRef))
              } else {
                Seq.empty
              }
            case _ =>
              Seq.empty
          }

          (srcname, targetRef)
        }

        val allbases = tpeBases(i).filterNot(_.takesTypeArgs)
        srcname ++ allbases.map(b => (targetRef, makeRef(b)))
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
            .filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)

    unparameterizedInheritanceData
  }

  private def makeStableBases(tpe: Type, allReferenceComponents: Set[Type]): Set[(AbstractReference, AbstractReference)] = {
    allReferenceComponents.flatMap {
      component =>
        val appliedBases = tpeBases(component).filterNot(_.takesTypeArgs)

        appliedBases.map {
          base =>
            val args = makeLambdaParams(None, tpe.etaExpand.typeParams).toMap
            val outRef = makeRef(base, args, forceLam = args.nonEmpty) match {
              case l: Lambda =>
                if (l.allArgumentsReferenced) {
                  l
                } else {
                  l.output
                }
              case reference: AppliedReference =>
                reference
            }
            val componentRef = makeRef(component)
            (componentRef, outRef)
        }
    }
  }

  private def makeBaseClasses(tpe: Type): Seq[(AbstractReference, AbstractReference)] = {
    def makeBaseLambdas(tpe: Type): Seq[AbstractReference] = {
      val basetypes = tpe
        .baseClasses
        .map(tpe.baseType)
        .filterNot(_.typeSymbol.fullName == tpe.typeSymbol.fullName)

      val targs = tpe.etaExpand.typeParams

      val lambdas = if (targs.nonEmpty) {
        basetypes.flatMap {
          base =>
            val lamParams = makeLambdaParams(None, targs)
            val reference = makeRef(base, lamParams.toMap)

            reference match {
              case l: Lambda =>
                Seq(l)
              case reference: AppliedReference =>
                Seq(Lambda(lamParams.map(_._2), reference))
                  .filter(_.allArgumentsReferenced)
            }
        }
      } else {
        Seq.empty
      }
      lambdas
    }

    val unref = UniRefinement.breakRefinement(tpe, destroyTypeLambdas = true)

    val out = unref
      .intersectionComponents
      .flatMap {
        base =>
          val baseAsLambda = if (base.takesTypeArgs) {
            base
          } else {
            base.etaExpand
          }
          val tref = makeRef(baseAsLambda)
          val baseLambdas = makeBaseLambdas(baseAsLambda)
          val mappedLambdas = baseLambdas
            .collect {
              case l: Lambda =>
                (tref, l)
            }
          mappedLambdas
      }
      .toSeq

    out
  }

  private def tpeBases(tpe: Type): Seq[Type] = {
    val tpef = tpe.dealias.resultType
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
    allbases
  }

  private def makeRef(tpe: Type): AbstractReference = {
    if (withCache) {
      globalCache.synchronized(globalCache.get(tpe)) match {
        case null =>
          val ref = makeRef(tpe, Map.empty)
          globalCache.synchronized(globalCache.put(tpe, ref))
          ref
        case value =>
          value
      }
    } else {
      makeRef(tpe, Map.empty)
    }
  }

  private def makeRef(tpe: Type, terminalNames: Map[String, LambdaParameter], forceLam: Boolean = false): AbstractReference = {
    makeRef(0)(tpe, Set(tpe), terminalNames, forceLam)
  }

  private def makeRef(level: Int)(tpe: Type, path: Set[Type], terminalNames: Map[String, LambdaParameter], forceLam: Boolean): AbstractReference = {
    val thisLevel = logger.sub(level)

    def sub(tpe: Type, stop: Map[String, LambdaParameter] = Map.empty): AbstractReference = {
      this.makeRef(level + 1)(tpe, path + tpe, terminalNames ++ stop, forceLam = false)
    }

    def makeBoundaries(t: Type): Boundaries = {
      t.typeSymbol.typeSignature match {
        case b: TypeBoundsApi =>
          if ((b.lo =:= nothing && b.hi =:= any) || (path.contains(b.lo) || path.contains(b.hi))) {
            Boundaries.Empty
          } else {
            Boundaries.Defined(sub(b.lo), sub(b.hi))
          }
        case _ =>
          Boundaries.Empty
      }
    }

    def makeLambda(t: Type): AbstractReference = {
      val asPoly = t.etaExpand
      val result = asPoly.resultType.dealias

      val targs = asPoly.typeParams
      val ctxId = if (level > 0) {
        Some(level.toString)
      } else {
        None
      }
      val lamParams = makeLambdaParams(ctxId, targs)

      thisLevel.log(s"✴️ λ type $t has parameters $lamParams, terminal names = $terminalNames")
      val reference = sub(result, lamParams.toMap)
      val out = Lambda(lamParams.map(_._2), reference)
      if (!out.allArgumentsReferenced) {
        val kvParams = lamParams.map { case (k, v) => s"$v = $k" }
        thisLevel.log(
          s"⚠️ unused 𝝺 args! type $t => $out, context: $terminalNames, 𝝺 params: $kvParams, 𝝺 result: $result => $reference, referenced: ${out.referenced} "
        )
      }

      thisLevel.log(s"✳️ Restored $t => $out")
      out
    }

    def unpack(t: Type, rules: Map[String, LambdaParameter]): AppliedNamedReference = {
      val tpef = norm(t.dealias.resultType)
      val prefix = getPrefix(tpef)
      val typeSymbol = tpef.typeSymbol
      val boundaries = makeBoundaries(tpef)
      val nameref = rules.get(typeSymbol.fullName) match {
        case Some(value) =>
          // this is a previously encountered type variable
          NameReference(SymTypeName(value.name), boundaries, prefix)

        case None =>
          makeNameReference(t, typeSymbol, boundaries, prefix)
      }

      tpef.typeArgs match {
        case Nil =>
          nameref

        case args =>
          val params = args.zip(t.dealias.typeConstructor.typeParams).map {
            case (a, pa) =>
              TypeParam(sub(a), getVariance(pa.asType))
          }
          FullReference(nameref.ref.name, params, prefix)
      }
    }

    def unpackRefined(t: Type, rules: Map[String, LambdaParameter]): AppliedReference = {
      UniRefinement.breakRefinement(t, destroyTypeLambdas = true) match {
        case Broken.Compound(tpes, decls) =>
          val parts = tpes.map(p => unpack(p, rules): AppliedReference)

          val intersection = LightTypeTagRef.maybeIntersection(parts)

          if (decls.nonEmpty) {
            Refinement(intersection, UniRefinement.convertDecls(decls.toList, rules).to(SortedSet))
          } else {
            intersection
          }

        case Broken.Single(t1) =>
          t match {
            case p if p.takesTypeArgs =>
              // we intentionally ignore breakRefinement result here, it breaks lambdas (with destroyTypeLambdas=true)
              unpack(p, rules)
            case _ =>
              unpack(t1, rules)
          }
      }
    }

    val out = tpe match {
      case p if forceLam =>
        Lambda(terminalNames.values.toList, unpackRefined(p, terminalNames))

      case _: PolyTypeApi =>
        // PolyType is not a type, we have to use tpe
        makeLambda(tpe)
      case p if p.takesTypeArgs =>
        if (terminalNames.contains(p.typeSymbol.fullName)) {
          unpackRefined(p, terminalNames)
        } else {
          makeLambda(p)
        }

      case c =>
        unpackRefined(c, terminalNames)
    }

    out
  }

  private def makeLambdaParams(ctxid: Option[String], targs: List[Symbol]): List[(String, LambdaParameter)] = {
    targs.zipWithIndex.map {
      case (targ, idx) =>
        val name = ctxid match {
          case Some(value) =>
            s"$value:${idx.toString}"
          case None =>
            idx.toString
        }

        targ.fullName -> LambdaParameter(name)
    }
  }

  private[reflect] object UniRefinement {
    def unapply(tpef: Type): Option[(List[Type], List[SymbolApi])] = {
      (tpef: AnyRef) match {
        case x: it.RefinementTypeRef =>
          Some((x.parents.map(_.asInstanceOf[Type]), x.decls.toList.asInstanceOf[List[SymbolApi]]))
        case r: RefinedTypeApi @unchecked =>
          Some((r.parents, r.decls.toList))
        case _ =>
          None
      }
    }

    def convertDecls(decls: List[SymbolApi], terminalNames: Map[String, LambdaParameter]): List[RefinementDecl] = {
      decls.flatMap {
        decl =>
          if (decl.isMethod) {
            val m = decl.asMethod
            val ret = m.returnType

            val params = m.paramLists.map {
              paramlist =>
                paramlist.map {
                  p =>
                    val pt = typeOfParam(p)
                    makeRef(pt, terminalNames).asInstanceOf[AppliedReference]
                }
            }

            val inputs = if (params.nonEmpty) {
              params
            } else {
              Seq(Seq.empty)
            }

            inputs.map {
              pl =>
                RefinementDecl.Signature(m.name.decodedName.toString, pl.toList, makeRef(ret, terminalNames).asInstanceOf[AppliedReference])
            }
          } else if (decl.isType) {
            val tpe = typeOfTypeMember(decl)
            val ref = makeRef(tpe, terminalNames)
            Seq(TypeMember(decl.name.decodedName.toString, ref))
          } else {
            None
          }
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

    def breakRefinement(t0: Type, destroyTypeLambdas: Boolean): Broken[Type, SymbolApi] = {
      breakRefinement0(destroyTypeLambdas)(t0) match {
        case (t, d) if d.isEmpty && t.size == 1 =>
          Broken.Single(t.head)
        case (t, d) =>
          logger.log(s"Found compound type parents=$t decls=$d")
          Broken.Compound(t, d)
      }
    }

    private def breakRefinement0(destroyTypeLambdas: Boolean)(t0: Type): (Set[Type], Set[SymbolApi]) = {
      fullNormDealiasResultType(t0, destroyTypeLambdas) match {
        case UniRefinement(parents, decls) =>
          val parts = parents.map(breakRefinement0(destroyTypeLambdas))
          val types = parts.flatMap(_._1)
          val partsDecls = parts.flatMap(_._2)
          (types.toSet, (decls ++ partsDecls).toSet)
        case t =>
          (Set(t), Set.empty)
      }
    }

  }

  private def fullNormDealiasResultType(t0: Type, destroyTypeLambdas: Boolean): Type = {
    val t1 = norm(t0.dealias)
    val t = if (destroyTypeLambdas && t1.takesTypeArgs) {
      val expanded = t1.etaExpand
      logger.log(s"Eta-expanded $t1 to $expanded")
      expanded
    } else {
      t1
    }
    var prev = null: Type
    var cur = t
    while (cur ne prev) {
      prev = cur
      cur = prev.dealias.resultType
    }
    norm(cur)
  }

  private def fullNormDealias(t0: Type): Type = {
    var prev = null: Type
    var cur = t0
    while (cur ne prev) {
      prev = cur
      cur = norm(prev.dealias).dealias
    }
    cur
  }

  private def getPrefix(tpef: Type): Option[AppliedReference] = {

    def fromRef(o: Type): Option[AppliedReference] = {
      makeRef(o) match {
        case a: AppliedReference =>
          Some(a)
        case o =>
          throw new IllegalStateException(s"Cannot extract prefix from $tpef: expected applied reference, but got $o")
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
              if (tpef.termSymbol != NoSymbol) {
                fromRef(tpef.termSymbol.owner.asType.toType)
              } else {
                fromRef(tpef.typeSymbol.owner.asType.toType)
              }

          }
        case k if k.termSymbol != NoSymbol =>
          val finalSymbol = dealiasSingletons(k.termSymbol)
          val name = symName(finalSymbol)
          val prePrefix = getPrefix(finalSymbol.typeSignature.finalResultType)
          Some(NameReference(name, Boundaries.Empty, prePrefix))
        case o =>
          fromRef(o)
      }
    }

    val prefix = getPre(tpef)
    val unpacked = prefix.flatMap(unpackPrefix)
    unpacked
  }

  @tailrec private def getPre(tpe: Type): Option[Type] = {
    tpe match {
      case t: TypeRefApi => Some(t.pre).filterNot(_ == NoPrefix)
      case t: SingleTypeApi => Some(t.pre).filterNot(_ == NoPrefix)
      case t: ExistentialTypeApi => getPre(t.underlying)
      case _ => None
    }
  }

  private def makeNameReference(originalType: Type, typeSymbol: Symbol, boundaries: Boundaries, prefix: Option[AppliedReference]): NameReference = {
    originalType match {
      case c: ConstantTypeApi =>
        NameReference(SymLiteral(c.value.value), boundaries, prefix)
      case s: SingleTypeApi if s.sym != NoSymbol =>
        val sym = dealiasSingletons(s.termSymbol)
        val resultType = sym.typeSignatureIn(s.pre).finalResultType
        val newPrefix = if (isSingletonType(resultType.typeSymbol)) getPrefix(resultType) else prefix
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

    if (isSingletonType(sym)) {
      SymTermName(base)
    } else {
      SymTypeName(base)
    }
  }

  @tailrec
  private def dealiasSingletons(termSymbol: Symbol): Symbol = {
    val resultTerm = termSymbol.typeSignature.finalResultType.termSymbol
    if (isSingletonType(resultTerm)) {
      dealiasSingletons(resultTerm)
    } else {
      termSymbol
    }
  }

  private def isSingletonType(sym: Symbol): Boolean = {
    sym.isTerm || sym.isModuleClass || (sym.typeSignature.isInstanceOf[SingletonTypeApi] && !sym.typeSignature.isInstanceOf[ThisTypeApi])
  }

  /** Mini `normalize`. We don't wanna do scary things such as beta-reduce. And AFAIK the only case that can make us
    * confuse a type-parameter for a non-parameter is an empty refinement `T {}`. So we just strip it when we get it.
    */
  @tailrec
  // ReflectionUtil.norm but with added logging
  protected[this] def norm(x: Type): Type = {
    x match {
      case RefinedType(t :: Nil, m) if m.isEmpty =>
        logger.log(s"Stripped empty refinement of type $t. member scope $m")
        norm(t)
      case AnnotatedType(_, t) =>
        norm(t)
      case _ => x
    }
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
