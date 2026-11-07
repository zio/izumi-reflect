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
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.reflect.api.Universe

private[macrortti] object LightTypeTagImplSupport {

  sealed trait Broken[T, S] {
    def intersectionComponents: Set[T]
    def decls: Set[S]
    def maybeUnbrokenType: Option[T]
  }

  object Broken {

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

private[macrortti] abstract class LightTypeTagImplSupport[U <: Universe with Singleton](
  val u: U,
  protected val logger: TrivialLogger
) {

  import LightTypeTagImplSupport.Broken
  import u._

  @inline protected final val any: Type = definitions.AnyTpe
  @inline protected final val obj: Type = definitions.ObjectTpe
  @inline protected final val nothing: Type = definitions.NothingTpe
  @inline protected final val ignored: Set[Type] = Set(any, obj, nothing)

  protected final case class RefContext(
    level: Int,
    nestedIn: Set[Type],
    terminalNames: Map[String, SymName.LambdaParamName],
    knownWildcards: Set[Symbol]
  ) {
    def advance(tpe: Type, extraRules: Map[String, SymName.LambdaParamName], allWildcards: Set[Symbol]): RefContext = {
      RefContext(level + 1, nestedIn + tpe, terminalNames ++ extraRules, allWildcards)
    }
  }

  protected final case class TargetSpec(tpe: Type, isLambdaOutput: Boolean)

  protected final def collectClassTypeMembers(tpe0: Type): List[(String, Type)] = {
    val targetTpe = Dealias.fullNormDealiasSquashHKTToPolyTypeResultType(tpe0)
    if (!targetTpe.typeSymbol.isClass || UniRefinement.unapply(targetTpe).isDefined) {
      Nil
    } else {
      val seen = mutable.HashSet.empty[String]
      targetTpe.baseClasses
        .filter(isNonStdlibClass)
        .flatMap(_.asClass.toType.decls)
        .filter(isInspectableTypeMember)
        .filter(decl => seen.add(decl.name.decodedName.toString))
        .map(decl => decl.name.decodedName.toString -> resolveClassTypeMember(targetTpe, decl))
    }
  }

  protected final def extractClassMemberComponentTypes(tpe: Type): List[Type] = {
    collectClassTypeMembers(tpe).flatMap {
      case (_, TypeBounds(lo, hi)) =>
        List(lo, hi).filterNot(ignored)
      case (_, memberSig) =>
        if (!memberSig.typeSymbol.isParameter && !ignored(memberSig)) List(memberSig) else Nil
    }
  }

  private[this] def isNonStdlibClass(sym: Symbol): Boolean = {
    sym.isClass && {
      val fullName = sym.fullName
      !fullName.startsWith("scala.") && !fullName.startsWith("java.")
    }
  }

  private[this] def isInspectableTypeMember(decl: Symbol): Boolean = {
    val isCandidateType = decl.isType && !decl.isClass && !decl.isParameter
    val isAccessibleUserMember = decl.isPublic && !decl.isSynthetic
    isCandidateType && isAccessibleUserMember && !decl.typeSignature.takesTypeArgs
  }

  private[this] def resolveClassTypeMember(targetTpe: Type, decl: Symbol): Type = {
    val sigInTarget = decl.typeSignatureIn(targetTpe)
    if (decl.isAbstract) sigInTarget else Dealias.fullNormDealias(sigInTarget)
  }

  protected final def makeLambdaParams(ctxIdx: Option[Int], tparams: List[Symbol]): List[(String, SymName.LambdaParamName)] = {
    tparams.zipWithIndex.map {
      case (tparamSym, idx) =>
        tparamSym.fullName -> SymName.LambdaParamName(idx, ctxIdx.getOrElse(LightTypeTagRef.LambdaConstants.defaultContextId), tparams.size)
    }
  }

  protected final def makeSymName(sym: Symbol): SymName = {
    val o = sym.owner
    val base = if (o.asInstanceOf[{ def hasMeaninglessName: Boolean }].hasMeaninglessName) {
      sym.name.decodedName.toString
    } else {
      sym.fullName
    }

    if (hasSingletonType(sym)) SymTermName(base) else SymTypeName(base)
  }

  protected final def makeVariance(tpes: TypeSymbol): Variance = {
    if (tpes.isCovariant) {
      Variance.Covariant
    } else if (tpes.isContravariant) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }

  protected final def isHKTOrPolyTypeOrResultTypeArtifact(tpe: Type): Boolean = {
    isHKTOrPolyType(tpe) || (!ReflectionUtil.isAbstractType(tpe) && tpe.typeArgs.exists(isTypeParamRefArtifact))
  }

  private[this] def isTypeParamRefArtifact(targ: Type): Boolean = {
    val tSym = targ.typeSymbol
    val isParamTypeSym = tSym.isParameter && tSym.isType
    targ.isInstanceOf[Universe#TypeRefApi] && isParamTypeSym && !ReflectionUtil.isIdentityLikeTypeLambda(targ)
  }

  protected final def isHKTOrPolyType(tpe: Type): Boolean = {
    tpe.takesTypeArgs || tpe.isInstanceOf[PolyTypeApi]
  }

  protected final def isExistentialArtifact(tpe: Type): Boolean = {
    tpe.typeArgs.exists {
      targ =>
        val tSym = targ.typeSymbol
        tSym.isType && tSym.asType.isExistential
    }
  }

  protected final def hasSingletonType(sym: Symbol): Boolean = {
    sym.isTerm || sym.isModuleClass || isSingletonType(sym.typeSignature)
  }

  @inline protected final def isSingletonType(tpe: Type): Boolean = {
    tpe.isInstanceOf[SingletonTypeApi] && !tpe.isInstanceOf[ThisTypeApi]
  }

  protected object UniRefinement {

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
          val types = parts.flatMap(_._1)
          val partsDecls = parts.flatMap(_._2)
          (types.toSet, (decls ++ partsDecls).toSet)
        case t =>
          (Set(t), Set.empty)
      }
    }

    def typeOfParam(p: Symbol): Type = {
      p.typeSignature
    }

    def typeOfTypeMember(decl: Symbol): Type = {
      if (decl.isAbstract) {
        decl.typeSignature match {
          case _: PolyTypeApi =>
            internal.typeBounds(
              lo = decl.typeSignature.map { case TypeBounds(lo, _) => lo; case t => t },
              hi = decl.typeSignature.map { case TypeBounds(_, hi) => hi; case t => t }
            )
          case other =>
            other
        }
      } else {
        decl.typeSignature
      }
    }

    def concreteTypesOfTypeMemberOnly(decl: Symbol): List[Type] = {
      if (decl.isAbstract) {
        decl.typeSignature match {
          case TypeBounds(lo, hi) => List(lo, hi)
          case _ => Nil
        }
      } else {
        List(decl.typeSignature)
      }
    }

  }

  protected object Dealias {

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

}
