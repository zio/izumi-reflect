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

import izumi.reflect.ReflectionUtil
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macrortti.LightTypeTagImpl.Broken
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymLiteral, SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.reflect.api.Universe

private[macrortti] trait LightTypeTagImplHelpers[U <: Universe with Singleton] {
  val u: U
  protected def logger: TrivialLogger
  protected def makeRef(tpe: u.Type): AbstractReference

  import u._

  @inline protected[this] final val any = definitions.AnyTpe
  @inline protected[this] final val obj = definitions.ObjectTpe
  @inline protected[this] final val nothing = definitions.NothingTpe
  @inline protected[this] final val ignored = Set(any, obj, nothing)

  protected[this] final case class RefBuildCtx(
    level: Int,
    ambientTpe: Type,
    nestedIn: Set[Type],
    terminalNames: Map[String, SymName.LambdaParamName],
    knownWildcards: Set[Symbol]
  ) {
    def advance(tpe: Type, stop: Map[String, SymName.LambdaParamName], allWildcards: Set[Symbol]): RefBuildCtx = {
      RefBuildCtx(level + 1, tpe, nestedIn + tpe, terminalNames ++ stop, allWildcards)
    }
  }

  protected[this] def isInspectableClassSym(sym: Symbol): Boolean = {
    if (!sym.isClass || sym.isModuleClass) {
      false
    } else if (sym.isPackage || sym.isPackageClass) {
      false
    } else {
      val name = sym.fullName
      !name.startsWith("scala.") && !name.startsWith("java.")
    }
  }

  protected[this] def extractClassTypeMemberDecls(tpe0: Type): List[(Symbol, Type)] = {
    val tpe = Dealias.fullNormDealias(tpe0)
    val sym = tpe.typeSymbol
    if (isHKTOrPolyType(tpe) || !isInspectableClassSym(sym)) {
      Nil
    } else {
      val seen = mutable.HashSet.empty[Name]
      tpe.baseClasses.iterator
        .filter(isInspectableClassSym)
        .flatMap(_.info.decls.iterator)
        .filter(d => isInspectableTypeMemberSym(d, seen))
        .map(d => (d, d.typeSignatureIn(tpe)))
        .toList
    }
  }

  private[this] def isInspectableTypeMemberSym(d: Symbol, seen: mutable.HashSet[Name]): Boolean = {
    if (!d.isType || d.asType.isParam) {
      false
    } else if (d.isClass || !d.isPublic) {
      false
    } else {
      seen.add(d.name)
    }
  }

  protected[this] def tpeBases(t0: Type): List[Type] = {
    val tpe = Dealias.fullNormDealias(t0)
    val upperBound = extractHigherKindedUpperBound(tpe)
    val isSingleton = isSingletonType(tpe)

    (upperBound.iterator ++ tpe.baseClasses.iterator.map(tpe.baseType))
      .filterNot(ignored)
      .filterNot(b => !isSingleton && b.typeSymbol.fullName == tpe.typeSymbol.fullName)
      .filterNot(_ =:= tpe)
      .toList
  }

  private[this] def extractHigherKindedUpperBound(tpe: Type): List[Type] = {
    val tpeSig = tpe.typeSymbol.typeSignature
    tpeSig.finalResultType match {
      case b: TypeBoundsApi if tpeSig.takesTypeArgs =>
        List(b.hi)
      case _ =>
        Nil
    }
  }

  protected[this] def makeLambdaParams(ctxIdx: Option[Int], tparams: List[Symbol]): List[(String, SymName.LambdaParamName)] = {
    val depth = ctxIdx.getOrElse(LightTypeTagRef.LambdaConstants.defaultContextId)
    val arity = tparams.size
    tparams.zipWithIndex.map {
      case (tparamSym, idx) =>
        tparamSym.fullName -> SymName.LambdaParamName(idx, depth, arity)
    }
  }

  protected[this] def makeNameReference(
    originalType: Type,
    boundaries: Boundaries,
    prefix: Option[AppliedReference]
  ): NameReference = {
    originalType match {
      case c: ConstantTypeApi =>
        NameReference(SymLiteral(c.value.value), boundaries, None)
      case s: SingleTypeApi if s.sym != NoSymbol =>
        makeSingleTypeNameReference(s, boundaries, prefix)
      case _ =>
        NameReference(makeSymName(originalType.typeSymbol), boundaries, prefix)
    }
  }

  private[this] def makeSingleTypeNameReference(
    s: SingleTypeApi,
    boundaries: Boundaries,
    prefix: Option[AppliedReference]
  ): NameReference = {
    s.sym.info.finalResultType match {
      case c: ConstantTypeApi =>
        NameReference(SymLiteral(c.value.value), boundaries, None)
      case _ =>
        val (sym, _) = Dealias.dealiasSingletons(s.termSymbol, s)
        val resultType = Dealias.fullNormDealias(sym.typeSignatureIn(s.pre).finalResultType)
        val newPrefix = if (hasSingletonType(resultType.typeSymbol)) makePrefixReference(resultType) else prefix
        NameReference(makeSymName(sym), boundaries, newPrefix)
    }
  }

  protected[this] def makePrefixReference(originalType: Type): Option[AppliedReference] = {
    extractPrefix(originalType).flatMap(unpackPrefix(originalType, _))
  }

  @tailrec
  private[this] def extractPrefix(t0: Type): Option[Type] = {
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
        unpackThisTypePrefix(originalType, k)
      case k if k.termSymbol != NoSymbol =>
        handleSingletonType(k)
      case o =>
        fromPrefixRef(originalType, o)
    }
  }

  private[this] def unpackThisTypePrefix(originalType: Type, k: ThisTypeApi): Option[AppliedReference] = {
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

  protected[this] def makeSymName(sym: Symbol): SymName = {
    val o = sym.owner
    val base = if (o.asInstanceOf[{ def hasMeaninglessName: Boolean }].hasMeaninglessName) {
      sym.name.decodedName.toString
    } else {
      sym.fullName
    }
    if (hasSingletonType(sym)) SymTermName(base) else SymTypeName(base)
  }

  protected[this] def makeVariance(tpes: TypeSymbol): Variance = {
    if (tpes.isCovariant) {
      Variance.Covariant
    } else if (tpes.isContravariant) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }

  protected[this] object UniRefinement {
    def unapply(tpe: Type): Option[(List[Type], List[Symbol])] = {
      (tpe: AnyRef) match {
        case x: scala.reflect.internal.Types#RefinementTypeRef =>
          Some((x.parents.asInstanceOf[List[Type]], x.decls.toList.asInstanceOf[List[Symbol]]))
        case r: RefinedTypeApi @unchecked =>
          Some((r.parents, r.decls.toList))
        case _ =>
          None
      }
    }

    def breakRefinement(t0: Type, squashHKTRefToPolyTypeResultType: Boolean): Broken[Type, Symbol] = {
      breakRefinement0(t0, squashHKTRefToPolyTypeResultType) match {
        case (t, d) if d.isEmpty && t.size == 1 =>
          Broken.Single(t.head)
        case (t, d) =>
          logger.log(s"Found compound type parents=$t decls=$d")
          Broken.Compound(t, d)
      }
    }

    private[this] def breakRefinement0(t0: Type, squashHKTRefToPolyTypeResultType: Boolean): (Set[Type], Set[Symbol]) = {
      val normalized = if (squashHKTRefToPolyTypeResultType) {
        Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(t0)
      } else {
        Dealias.fullNormDealias(t0)
      }
      normalized match {
        case UniRefinement(parents, decls) =>
          val parts = parents.map(breakRefinement0(_, squashHKTRefToPolyTypeResultType))
          (parts.flatMap(_._1).toSet, (decls ++ parts.flatMap(_._2)).toSet)
        case t =>
          (Set(t), Set.empty)
      }
    }

    def typeOfParam(p: Symbol): Type = p.typeSignature

    def typeOfTypeMember(decl: Symbol): Type = typeOfTypeMemberSig(decl, decl.typeSignature)

    def typeOfTypeMemberSig(decl: Symbol, sig: Type): Type = {
      if (decl.isAbstract) {
        sig match {
          case _: PolyTypeApi =>
            internal.typeBounds(
              lo = sig.map { case TypeBounds(lo, _) => lo; case t => t },
              hi = sig.map { case TypeBounds(_, hi) => hi; case t => t }
            )
          case _ =>
            sig
        }
      } else {
        sig
      }
    }

    def concreteTypesOfTypeMemberOnly(decl: Symbol): List[Type] = {
      concreteTypesOfTypeMemberSig(decl, decl.typeSignature)
    }

    def concreteTypesOfTypeMemberSig(decl: Symbol, sig: Type): List[Type] = {
      val raw = if (decl.isAbstract) {
        sig match {
          case TypeBounds(lo, hi) => List(lo, hi)
          case _ => Nil
        }
      } else if (sig.takesTypeArgs || sig.isInstanceOf[PolyTypeApi]) {
        Nil
      } else {
        List(sig)
      }
      raw.filterNot(t => ignored(t) || t.typeSymbol.isParameter)
    }
  }

  protected[this] object Dealias {
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
      scala211ExistentialDealiasWorkaround(t0)
    }

    @tailrec
    private[this] def scala211ExistentialDealiasWorkaround(t0: Type): Type = {
      t0 match {
        case existential: ExistentialTypeApi =>
          internal.existentialType(existential.quantified, scala211ExistentialDealiasWorkaroundLoop(existential.underlying.dealias))
        case t =>
          val next = norm(t).dealias
          if (next eq t) t else scala211ExistentialDealiasWorkaround(next)
      }
    }

    private[this] def scala211ExistentialDealiasWorkaroundLoop(t0: Type): Type = scala211ExistentialDealiasWorkaround(t0)

    @tailrec
    def dealiasSingletons(termSymbol: Symbol, termSymbolTpe: Type): (Symbol, Type) = {
      val resultTermTpe = termSymbol.typeSignature.finalResultType
      val resultTerm = resultTermTpe.termSymbol

      if (hasSingletonType(resultTerm)) {
        dealiasSingletons(resultTerm, resultTermTpe)
      } else {
        (termSymbol, termSymbolTpe)
      }
    }

    @inline def norm(x: Type): Type = {
      ReflectionUtil.norm(u: u.type, logger)(x)
    }
  }

  protected[this] def isHKTOrPolyTypeOrResultTypeArtifact(tpe: Type): Boolean = {
    isHKTOrPolyType(tpe) || (!ReflectionUtil.isAbstractType(tpe) && tpe.typeArgs.exists(isTypeParamRefArtifact))
  }

  private[this] def isTypeParamRefArtifact(targ: Type): Boolean = {
    val tSym = targ.typeSymbol
    if (!targ.isInstanceOf[Universe#TypeRefApi]) {
      false
    } else if (!tSym.isParameter || !tSym.isType) {
      false
    } else {
      !ReflectionUtil.isIdentityLikeTypeLambda(targ)
    }
  }

  protected[this] def isHKTOrPolyType(tpe: Type): Boolean = {
    tpe.takesTypeArgs || tpe.isInstanceOf[PolyTypeApi]
  }

  protected[this] def isExistentialArtifact(tpe: Type): Boolean = {
    tpe.typeArgs.exists {
      targ =>
        val tSym = targ.typeSymbol
        tSym.isType && tSym.asType.isExistential
    }
  }

  protected[this] def hasSingletonType(sym: Symbol): Boolean = {
    sym.isTerm || sym.isModuleClass || isSingletonType(sym.typeSignature)
  }

  @inline protected[this] def isSingletonType(tpe: Type): Boolean = {
    tpe.isInstanceOf[SingletonTypeApi] && !tpe.isInstanceOf[ThisTypeApi]
  }
}
