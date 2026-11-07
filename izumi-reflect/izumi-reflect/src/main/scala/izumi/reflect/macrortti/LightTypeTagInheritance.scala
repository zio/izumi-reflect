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

import izumi.reflect.internal.fundamentals.collections.ImmutableMultiMap
import izumi.reflect.internal.fundamentals.platform.basics.IzBoolean._
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config
import izumi.reflect.macrortti.LightTypeTagInheritance._
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymTypeName
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable

object LightTypeTagInheritance {
  private[reflect] final val tpeNothing = NameReference(SymTypeName("scala.Nothing"))
  private[reflect] final val tpeNull = NameReference(SymTypeName("scala.Null"))

  private[reflect] final val tpeAny = NameReference(SymTypeName("scala.Any"))
  private[reflect] final val tpeAnyRef = NameReference(SymTypeName("scala.AnyRef"))

  private[reflect] final val tpeMatchable = NameReference(SymTypeName("scala.Matchable"))
  private[reflect] final val tpeObject = NameReference(SymTypeName(classOf[Object].getName))

  private final case class Ctx(
    logger: TrivialLogger,
    self: LightTypeTagInheritance
  ) {
    def next(): Ctx = Ctx(logger.sub(), self)
  }
  private implicit final class CtxExt(private val ctx: Ctx) extends AnyVal {
    def isChild(selfT0: LightTypeTagRef, thatT0: LightTypeTagRef): Boolean = ctx.self.isChild(ctx.next())(selfT0, thatT0)
  }
}

final class LightTypeTagInheritance(self: LightTypeTag, other: LightTypeTag) {
  private[this] lazy val basesdb: ImmutableMultiMap[AbstractReference, AbstractReference] = LightTypeTag.mergeIDBs(self.basesdb, other.basesdb)
  private[this] lazy val idb: ImmutableMultiMap[NameReference, NameReference] = LightTypeTag.mergeIDBs(self.idb, other.idb)

  def isChild(): Boolean = {
    val st = self.ref
    val ot = other.ref
    val logger = TrivialLogger.make[this.type](config = Config.console)

    logger.log(s"""⚙️ Inheritance check: ${self.repr} <?< ${other.repr}
                  |⚡️bases: ${LTTRenderables.Long.renderDb(basesdb)}
                  |⚡️inheritance: ${LTTRenderables.Long.renderDb(idb)}""".stripMargin)

    isChild(Ctx(logger, this))(st, ot)
  }

  private def isChild(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    import ctx._
    logger.log(s"✴️ isChild: `${selfT.repr}` <:< `${thatT.repr}`, context = $ctx")

    val result = if (isTrivialChild(selfT, thatT)) {
      true
    } else {
      isStructuralOrNominalChild(ctx)(selfT, thatT)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} isChild: `${selfT.repr}` <:< `${thatT.repr}` == $result")
    result
  }

  private def isTrivialChild(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    selfT == thatT || selfT == tpeNothing || selfT == tpeNull || isTopType(thatT)
  }

  private def isTopType(t: LightTypeTagRef): Boolean = {
    t == tpeAny || t == tpeAnyRef || t == tpeObject || t == tpeMatchable
  }

  private def isStructuralOrNominalChild(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    (selfT, thatT) match {
      case (s: WildcardReference, t) =>
        compareWildcardLHS(ctx)(s, t)
      case (s, t: WildcardReference) =>
        compareWildcardRHS(ctx)(s, t)
      case (s: AppliedNamedReference, t: AppliedNamedReference) =>
        compareNamedReferences(ctx)(s, t)
      case (s: Lambda, t: Lambda) =>
        s.input.size == t.input.size && isChild(ctx.next())(s.normalizedOutput, t.normalizedOutput)
      case (s: AppliedNamedReference, t: Lambda) =>
        isChild(ctx.next())(s, t.output)
      case (s: Lambda, t: AppliedNamedReference) =>
        isChild(ctx.next())(s.output, t)
      case _ =>
        compareCompoundReferences(ctx)(selfT, thatT)
    }
  }

  private def compareWildcardLHS(ctx: Ctx)(s: WildcardReference, thatT: LightTypeTagRef): Boolean = {
    thatT match {
      case t: WildcardReference =>
        s.boundaries match {
          case Boundaries.Defined(_, top) =>
            compareBounds(ctx)(top, t.boundaries)
          case Boundaries.Empty =>
            t.boundaries == Boundaries.Empty
        }
      case t =>
        s.boundaries match {
          case Boundaries.Defined(_, top) =>
            ctx.next().isChild(top, t)
          case Boundaries.Empty =>
            s == t
        }
    }
  }

  private def compareWildcardRHS(ctx: Ctx)(selfT: LightTypeTagRef, t: WildcardReference): Boolean = {
    selfT match {
      case s: AppliedNamedReference =>
        compareBounds(ctx)(s, t.boundaries)
      case s: Lambda =>
        isChild(ctx.next())(s.output, t)
      case s =>
        compareCompoundReferences(ctx)(s, t)
    }
  }

  private def compareNamedReferences(ctx: Ctx)(selfRef: AppliedNamedReference, thatRef: AppliedNamedReference): Boolean = {
    (selfRef, thatRef) match {
      case (s: FullReference, t: FullReference) =>
        compareParameterizedRefs(ctx)(s, t)
      case (s: FullReference, t: NameReference) =>
        compareFullToNameRef(ctx)(s, t)
      case (s: NameReference, t: FullReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
      case (s: NameReference, t: NameReference) =>
        compareNameToNameRef(ctx)(s, t)
    }
  }

  private def compareFullToNameRef(ctx: Ctx)(s: FullReference, t: NameReference): Boolean = {
    any(
      oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t),
      all(
        compareBounds(ctx)(s, t.boundaries),
        any(
          oneOfUnparameterizedParentsIsInheritedFrom(ctx)(s.asName, t),
          isInBoundsOfAnEqualBoundedAbstractType(s, t)
        )
      )
    )
  }

  private def compareNameToNameRef(ctx: Ctx)(s: NameReference, t: NameReference): Boolean = {
    val boundIsOk = compareBounds(ctx)(s, t.boundaries)
    any(
      boundIsOk && any(
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t),
        oneOfUnparameterizedParentsIsInheritedFrom(ctx)(s, t),
        isInBoundsOfAnEqualBoundedAbstractType(s, t)
      ),
      s.boundaries match {
        case Boundaries.Defined(_, sUp) =>
          ctx.isChild(sUp, t)
        case Boundaries.Empty =>
          false
      }
    )
  }

  private def compareCompoundReferences(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    (selfT, thatT) match {
      case (s: IntersectionReference, t: IntersectionReference) =>
        t.refs.forall(p => s.refs.exists(c => ctx.isChild(c, p)))
      case (s: IntersectionReference, t: Refinement) =>
        s.refs.exists(c => ctx.isChild(c, t)) || checkStructuralRefinementChild(ctx)(s, t)
      case (s: IntersectionReference, t: LightTypeTagRef) =>
        s.refs.exists(c => ctx.isChild(c, t))
      case (s: LightTypeTagRef, o: IntersectionReference) =>
        o.refs.forall(t => ctx.isChild(s, t))
      case (s: UnionReference, t: UnionReference) =>
        s.refs.forall(c => t.refs.exists(p => ctx.isChild(c, p)))
      case (s: UnionReference, t: LightTypeTagRef) =>
        s.refs.forall(c => ctx.isChild(c, t))
      case (s: LightTypeTagRef, o: UnionReference) =>
        o.refs.exists(t => ctx.isChild(s, t))
      case (s: Refinement, t: Refinement) =>
        ctx.isChild(s.reference, t.reference) && compareDecls(ctx.next())(collectDeclsOf(s), t.decls)
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t) || checkStructuralRefinementChild(ctx)(s, t)
    }
  }

  private def checkStructuralRefinementChild(ctx: Ctx)(s: AbstractReference, t: Refinement): Boolean = {
    ctx.isChild(s, t.reference) && {
      val sDecls = collectDeclsOf(s)
      sDecls.nonEmpty && compareDecls(ctx.next())(sDecls, t.decls)
    }
  }

  private def collectDeclsOf(ref: AbstractReference, visited: Set[AbstractReference] = Set.empty): Set[RefinementDecl] = {
    if (visited.contains(ref)) {
      Set.empty
    } else {
      val nextVisited = visited + ref
      val directDecls = extractDirectDecls(ref, nextVisited)
      val baseParents = parameterizedParentsOf(ref) ++ (ref match {
        case f: FullReference => appliedLambdaParentsOf(f)
        case _ => Nil
      })
      val ownBaseDecls = baseParents.collect { case Refinement(_, decls) => decls }.flatten
      val ancestorDecls = baseParents.collect {
        case other if !other.isInstanceOf[Refinement] => collectDeclsOf(other, nextVisited)
      }.flatten
      mergeDecls(mergeDecls(ancestorDecls, ownBaseDecls), directDecls)
    }
  }

  private def extractDirectDecls(ref: AbstractReference, nextVisited: Set[AbstractReference]): Set[RefinementDecl] = {
    ref match {
      case Refinement(base, decls) =>
        mergeDecls(collectDeclsOf(base, nextVisited), decls)
      case IntersectionReference(refs) =>
        refs.flatMap(collectDeclsOf(_, nextVisited))
      case _ =>
        Set.empty
    }
  }

  private def mergeDecls(parentDecls: Set[RefinementDecl], childDecls: Set[RefinementDecl]): Set[RefinementDecl] = {
    val childNames = childDecls.map(_.name)
    parentDecls.filterNot(d => childNames.contains(d.name)) ++ childDecls
  }

  private def compareBounds(ctx: Ctx)(s: AbstractReference, t: Boundaries): Boolean = {
    t match {
      case Boundaries.Defined(tLow, tUp) =>
        ctx.isChild(s, tUp) && ctx.isChild(tLow, s)
      case Boundaries.Empty =>
        true
    }
  }

  private def compareDecls(ctx: Ctx)(sDecls: Set[RefinementDecl], tDecls: Set[RefinementDecl]): Boolean = {
    val s = sDecls.groupBy(_.name)
    // for every decl on the right there's a <: decl on the left
    tDecls.forall {
      r =>
        val lOverrides = s.get(r.name).toSet.flatten
        lOverrides.exists(compareDecl(ctx)(_, r))
    }
  }

  private def compareDecl(ctx: Ctx)(s: RefinementDecl, t: RefinementDecl): Boolean = (s, t) match {
    case (
          RefinementDecl.TypeMember(ln, lref),
          RefinementDecl.TypeMember(rn, NameReference(SymName.SymTypeName(rn1), rBounds, None))
        ) if rn == rn1 =>
      // we're comparing two abstract types type X = X|>:A<:B|
      // We know that the type is abstract if its name matches the type member's name
      ln == rn && compareAbstractTypeMemberBounds(ctx)(ln, lref, rBounds)
    case (RefinementDecl.TypeMember(ln, lref), RefinementDecl.TypeMember(rn, rref)) =>
      // if the rhs type is not abstract (has form `type X = Int`), then lhs must be exactly equal to it, not <:
      ln == rn && lref == rref
    case (RefinementDecl.Signature(ln, lins, lout), RefinementDecl.Signature(rn, rins, rout)) =>
      (ln == rn
        && lins.iterator.zipAll(rins.iterator, null, null).forall {
          case (null, _) => false
          case (_, null) => false
          case (l, r) => ctx.isChild(r, l) // contravariant
        }
        && ctx.isChild(lout, rout)) // covariant
    case _ =>
      false
  }

  private def compareAbstractTypeMemberBounds(ctx: Ctx)(memberName: String, lref: AbstractReference, rBounds: Boundaries): Boolean = {
    (lref, rBounds) match {
      case (_, Boundaries.Empty) =>
        true
      case (NameReference(SymName.SymTypeName(ln1), Boundaries.Defined(lLow, lHigh), None), Boundaries.Defined(rLow, rHigh)) if ln1 == memberName =>
        ctx.isChild(lHigh, rHigh) && ctx.isChild(rLow, lLow)
      case _ =>
        compareBounds(ctx)(lref, rBounds)
    }
  }

  private def compareParameterizedRefs(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {
    def parametersConform: Boolean = {
      self.parameters.zipAll(that.parameters, null, null).forall {
        case (ps, pt) =>
          (ps ne null) && (pt ne null) && parameterConforms(ctx)(ps, pt)
      }
    }

    val selfNameRef = self.asName
    val thatNameRef = that.asName

    ctx
      .logger.log(
        s"⚠️ comparing parameterized references: `${self.repr}` <:< `${that.repr}`, paramsOk = $parametersConform, ctorsOk = ${selfNameRef == thatNameRef}, sameArity = ${self.parameters.size == that.parameters.size}, context = $ctx"
      )

    if (selfNameRef == thatNameRef) {
      parametersConform
    } else if (oneOfParameterizedParentsIsInheritedFrom(ctx)(self, that)) {
      true
    } else {
      val appliedLambdaParents = appliedLambdaParentsOf(self)
      ctx.logger.log {
        s"ℹ️ checking applied lambda parents of self=`${self.repr}`: parents=${appliedLambdaParents.map(_.repr)} <:< that=`${that.repr}`"
      }
      appliedLambdaParents.exists(ctx.isChild(_, that))
    }
  }

  private def parameterConforms(ctx: Ctx)(ps: TypeParam, pt: TypeParam): Boolean = {
    (pt.variance, pt.ref) match {
      case (Variance.Invariant, wc: LightTypeTagRef.WildcardReference) =>
        compareBounds(ctx)(ps.ref, wc.boundaries)
      case (Variance.Invariant, _) =>
        ps.ref == pt.ref
      case (Variance.Contravariant, wc: LightTypeTagRef.WildcardReference) =>
        contravariantWildcardConforms(ctx)(ps.ref, wc.boundaries)
      case (Variance.Contravariant, _) =>
        ctx.isChild(pt.ref, ps.ref)
      case (Variance.Covariant, wc: LightTypeTagRef.WildcardReference) =>
        covariantWildcardConforms(ctx)(ps.ref, wc.boundaries)
      case (Variance.Covariant, _) =>
        ctx.isChild(ps.ref, pt.ref)
    }
  }

  private def contravariantWildcardConforms(ctx: Ctx)(psRef: AbstractReference, boundaries: Boundaries): Boolean = {
    boundaries match {
      case Boundaries.Defined(bottom, _) => ctx.isChild(bottom, psRef)
      case Boundaries.Empty => true
    }
  }

  private def covariantWildcardConforms(ctx: Ctx)(psRef: AbstractReference, boundaries: Boundaries): Boolean = {
    boundaries match {
      case Boundaries.Defined(_, top) => ctx.isChild(psRef, top)
      case Boundaries.Empty => true
    }
  }

  private def appliedLambdaParentsOf(self: FullReference): List[AbstractReference] = {
    val selfNameRef = self.asName
    val selfNormalizedLambdaParamsSize = self.parameters.count(p => isFakeParam(p.ref))
    basesdb
      .iterator.collect {
        case (l: Lambda, lparents) if isSameNamedRef(l.output, selfNameRef) =>
          lparents.flatMap(instantiateLambdaParent(self, selfNormalizedLambdaParamsSize, _))
      }.flatten.toList
  }

  private def instantiateLambdaParent(
    self: FullReference,
    selfNormalizedLambdaParamsSize: Int,
    parent: AbstractReference
  ): List[AbstractReference] = {
    parent match {
      case lp: Lambda =>
        val scala2FullDbFormLambda = if (lp.input.size == self.parameters.size) {
          List(lp.combine(self.parameters.map(_.ref)))
        } else Nil
        val scala3FullDbFormLambda = if (lp.input.size == selfNormalizedLambdaParamsSize) {
          val ps = self.parameters.collect { case FakeParam(pRef) => pRef }
          val selfPs = ps.sortBy(_.ref.asInstanceOf[SymName.LambdaParamName].index)
          List(lp.combine(selfPs))
        } else Nil
        scala2FullDbFormLambda ++ scala3FullDbFormLambda
      case _ =>
        Nil
    }
  }

  private def isSameNamedRef(a: AbstractReference, b: AbstractReference): Boolean = {
    (a, b) match {
      case (an: AppliedNamedReference, ab: AppliedNamedReference) =>
        an.asName == ab.asName
      case _ =>
        false
    }
  }

  def isFakeParam(reference: LightTypeTagRef.AbstractReference): Boolean = reference match {
    case reference: NameReference =>
      reference.symName match {
        case l: SymName.LambdaParamName if l.depth == LightTypeTagRef.LambdaConstants.lambdaFakeParamDepth => true
        case _ => false
      }
    case _ => false
  }

  object FakeParam {
    def unapply(tparam: TypeParam): Option[LightTypeTagRef.NameReference] = {
      if (isFakeParam(tparam.ref)) Some(tparam.ref.asInstanceOf[NameReference]) else None
    }
  }

  private def parameterizedParentsOf(t: AbstractReference): Set[AbstractReference] = {
    val basesDbParents = basesdb.getOrElse(t, Set.empty)
    val withBoundaryParents = t match {
      case NameReference(_, b: Boundaries.Defined, _) =>
        basesDbParents + b.top
      case WildcardReference(b: Boundaries.Defined) =>
        basesDbParents + b.top
      case _ =>
        basesDbParents
    }
    withBoundaryParents
  }

  private def oneOfParameterizedParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
    val parents = parameterizedParentsOf(child)
    ctx.logger.log(s"Looking up parameterized parents of $child => $parents")
    parents.exists(ctx.isChild(_, parent))
  }

  private def oneOfUnparameterizedParentsIsInheritedFrom(ctx: Ctx)(child: NameReference, parent: NameReference): Boolean = {
    val parents = unparameterizedParentsOf(child)
    ctx.logger.log(s"Looking up unparameterized parents of $child => $parents")
    parents.exists(ctx.isChild(_, parent))
  }

  private def isInBoundsOfAnEqualBoundedAbstractType(child: AbstractReference, parent: NameReference): Boolean = {
    parent.boundaries match {
      case Boundaries.Defined(bottom, top) if top == bottom && top == child => true
      case _ => false
    }
  }

  private def unparameterizedParentsOf(t: NameReference): mutable.HashSet[NameReference] = {
    def parentsOf(t: NameReference, out: mutable.HashSet[NameReference], tested: mutable.HashSet[NameReference]): Unit = {
      val direct = idb.get(t).toSet.flatten
      tested += t
      out ++= direct

      val nextNames = direct.map(_.asName)
      nextNames
        .diff(tested)
        .foreach {
          b =>
            parentsOf(b.asName, out, tested)
        }
    }

    val out = mutable.HashSet[NameReference]()
    val tested = mutable.HashSet[NameReference]()
    parentsOf(t, out, tested)
    out
  }

}
