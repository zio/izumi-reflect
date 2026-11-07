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

  private final val topLevelTypes: Set[LightTypeTagRef] = Set(tpeAny, tpeAnyRef, tpeObject, tpeMatchable)

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
      dispatchSubtypeComparison(ctx)(selfT, thatT)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} isChild: `${selfT.repr}` <:< `${thatT.repr}` == $result")
    result
  }

  private def isTrivialChild(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    selfT == thatT || selfT == tpeNothing || selfT == tpeNull || topLevelTypes.contains(thatT)
  }

  private def dispatchSubtypeComparison(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    (selfT, thatT) match {
      case (s: WildcardReference, t) =>
        compareLeftWildcard(ctx)(s, t)
      case (s, t: WildcardReference) =>
        compareRightWildcard(ctx)(s, t)
      case (s: AppliedNamedReference, t: AppliedNamedReference) =>
        compareNamedRefs(ctx)(s, t)
      case (s: Lambda, t) =>
        compareLeftLambda(ctx)(s, t)
      case (s: AppliedNamedReference, t: Lambda) =>
        isChild(ctx.next())(s, t.output)
      case (s, t) =>
        compareCompoundRefs(ctx)(s, t)
    }
  }

  private def compareLeftWildcard(ctx: Ctx)(s: WildcardReference, thatT: LightTypeTagRef): Boolean = {
    (s.boundaries, thatT) match {
      case (Boundaries.Defined(_, top), t: WildcardReference) =>
        compareBounds(ctx)(top, t.boundaries)
      case (Boundaries.Empty, t: WildcardReference) =>
        t.boundaries == Boundaries.Empty
      case (Boundaries.Defined(_, top), t) =>
        ctx.next().isChild(top, t)
      case (Boundaries.Empty, t) =>
        s == t
    }
  }

  private def compareRightWildcard(ctx: Ctx)(selfT: LightTypeTagRef, t: WildcardReference): Boolean = {
    selfT match {
      case s: AppliedNamedReference =>
        compareBounds(ctx)(s, t.boundaries)
      case s: Lambda =>
        isChild(ctx.next())(s.output, t)
      case _ =>
        false
    }
  }

  private def compareNamedRefs(ctx: Ctx)(selfT: AppliedNamedReference, thatT: AppliedNamedReference): Boolean = {
    (selfT, thatT) match {
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

  private def compareLeftLambda(ctx: Ctx)(s: Lambda, thatT: LightTypeTagRef): Boolean = {
    thatT match {
      case _: AppliedNamedReference =>
        isChild(ctx.next())(s.output, thatT)
      case o: Lambda =>
        s.input.size == o.input.size && isChild(ctx.next())(s.normalizedOutput, o.normalizedOutput)
      case _ =>
        compareCompoundRefs(ctx)(s, thatT)
    }
  }

  private def compareCompoundRefs(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    (selfT, thatT) match {
      case (s: IntersectionReference, t: IntersectionReference) =>
        t.refs.forall(p => s.refs.exists(c => ctx.isChild(c, p)))
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
      case (s, t) =>
        compareRefinements(ctx)(s, t)
    }
  }

  private def compareRefinements(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    (selfT, thatT) match {
      case (s: Refinement, t: Refinement) =>
        ctx.isChild(s.reference, t.reference) && compareDecls(ctx.next())(s.decls, t.decls)
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: FullReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t) || hasAppliedLambdaParent(ctx)(s, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
      case _ =>
        false
    }
  }

  private def compareBounds(ctx: Ctx)(s: AbstractReference, t: Boundaries): Boolean = {
    t match {
      case Boundaries.Defined(tLow, tUp) =>
        ctx.isChild(s, tUp) && isLowerBoundSatisfied(ctx)(tLow, s)
      case Boundaries.Empty =>
        true
    }
  }

  private def isLowerBoundSatisfied(ctx: Ctx)(tLow: AbstractReference, s: AbstractReference): Boolean = {
    ctx.isChild(tLow, s) || (s match {
      case NameReference(_, Boundaries.Defined(sLow, _), _) =>
        ctx.isChild(tLow, sLow)
      case WildcardReference(Boundaries.Defined(sLow, _)) =>
        ctx.isChild(tLow, sLow)
      case _ =>
        false
    })
  }

  private def compareDecls(ctx: Ctx)(sDecls: Set[RefinementDecl], tDecls: Set[RefinementDecl]): Boolean = {
    val s = sDecls.groupBy(_.name)
    // Every declaration on the right-hand side must have a conforming override on the left-hand side.
    tDecls.forall {
      r =>
        val lOverrides = s.get(r.name).toSet.flatten
        lOverrides.exists(compareDecl(ctx)(_, r))
    }
  }

  private def compareDecl(ctx: Ctx)(s: RefinementDecl, t: RefinementDecl): Boolean = (s, t) match {
    case (RefinementDecl.TypeMember(ln, lref), RefinementDecl.TypeMember(rn, rref)) =>
      compareTypeMemberDecl(ctx)(ln -> lref, rn -> rref)
    case ( lSig: RefinementDecl.Signature, rSig: RefinementDecl.Signature) =>
      compareSignatureDecl(ctx)(lSig, rSig)
    case _ =>
      false
  }

  private def compareTypeMemberDecl(
    ctx: Ctx
  )(lhs: (String, AbstractReference),
    rhs: (String, AbstractReference)
  ): Boolean = {
    val (ln, lref) = lhs
    val (rn, rref) = rhs
    if (ln != rn) {
      false
    } else {
      extractAbstractTypeMemberBounds(rn, rref) match {
        case Some(rBounds) =>
          // Abstract type members match any declaration whose type or bounds conform to the target bounds.
          compareBounds(ctx)(lref, rBounds)
        case None =>
          // Concrete type member aliases are invariant and require exact structural equality.
          lref == rref
      }
    }
  }

  private def extractAbstractTypeMemberBounds(memberName: String, ref: AbstractReference): Option[Boundaries] = ref match {
    case NameReference(SymName.SymTypeName(symName), bounds, _)
        if symName == memberName || symName.endsWith(s".$memberName") =>
      Some(bounds)
    case _ =>
      None
  }

  private def compareSignatureDecl(ctx: Ctx)(lhs: RefinementDecl.Signature, rhs: RefinementDecl.Signature): Boolean = {
    val inputsConform = lhs.input.iterator.zipAll(rhs.input.iterator, null, null).forall {
      case (null, _) | (_, null) => false
      case (l, r) => ctx.isChild(r, l)
    }
    lhs.name == rhs.name && inputsConform && ctx.isChild(lhs.output, rhs.output)
  }

  private def compareParameterizedRefs(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {
    val paramsOk = parametersConform(ctx)(self, that)
    val selfNameRef = self.asName
    val thatNameRef = that.asName

    ctx
      .logger.log(
        s"⚠️ comparing parameterized references: `${self.repr}` <:< `${that.repr}`, paramsOk = $paramsOk, ctorsOk = ${selfNameRef == thatNameRef}, sameArity = ${self.parameters.size == that.parameters.size}, context = $ctx"
      )

    if (selfNameRef == thatNameRef) {
      paramsOk
    } else {
      oneOfParameterizedParentsIsInheritedFrom(ctx)(self, that) || hasAppliedLambdaParent(ctx)(self, that)
    }
  }

  private def parametersConform(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {
    self.parameters.zipAll(that.parameters, null, null).forall {
      case (ps, pt) =>
        (ps ne null) && (pt ne null) && parameterConforms(ctx)(ps.ref, pt)
    }
  }

  private def parameterConforms(ctx: Ctx)(selfParamRef: AbstractReference, thatParam: TypeParam): Boolean = {
    (thatParam.variance, thatParam.ref) match {
      case (Variance.Invariant, wc: WildcardReference) =>
        compareBounds(ctx)(selfParamRef, wc.boundaries)
      case (Variance.Invariant, ptRef) =>
        selfParamRef == ptRef
      case (Variance.Contravariant, wc: WildcardReference) =>
        contravariantWildcardConforms(ctx)(selfParamRef, wc.boundaries)
      case (Variance.Contravariant, ptRef) =>
        ctx.isChild(ptRef, selfParamRef)
      case (Variance.Covariant, wc: WildcardReference) =>
        covariantWildcardConforms(ctx)(selfParamRef, wc.boundaries)
      case (Variance.Covariant, ptRef) =>
        ctx.isChild(selfParamRef, ptRef)
    }
  }

  private def contravariantWildcardConforms(ctx: Ctx)(selfParamRef: AbstractReference, bounds: Boundaries): Boolean = {
    bounds match {
      case Boundaries.Defined(bottom, _) => ctx.isChild(bottom, selfParamRef)
      case Boundaries.Empty => true
    }
  }

  private def covariantWildcardConforms(ctx: Ctx)(selfParamRef: AbstractReference, bounds: Boundaries): Boolean = {
    bounds match {
      case Boundaries.Defined(_, top) => ctx.isChild(selfParamRef, top)
      case Boundaries.Empty => true
    }
  }

  private def hasAppliedLambdaParent(ctx: Ctx)(self: FullReference, that: AbstractReference): Boolean = {
    val appliedLambdaParents = appliedLambdaParentsFor(self)
    ctx.logger.log {
      s"ℹ️ checking applied lambda parents of self=`${self.repr}`: parents=${appliedLambdaParents.map(_.repr)} <:< that=`${that.repr}`"
    }
    appliedLambdaParents.exists(ctx.isChild(_, that))
  }

  private def appliedLambdaParentsFor(self: FullReference): List[AbstractReference] = {
    val selfNameRef = self.asName
    val selfNormalizedLambdaParamsSize = self.parameters.count(p => isFakeParam(p.ref))
    basesdb
      .iterator.collect {
        case (l: Lambda, lparents) if isSameNamedRef(l.output, selfNameRef) =>
          lparents.flatMap(combineLambdaParent(self, selfNormalizedLambdaParamsSize))
      }.flatten.toList
  }

  private def combineLambdaParent(
    self: FullReference,
    selfNormalizedLambdaParamsSize: Int
  )(candidate: AbstractReference
  ): List[AbstractReference] = {
    candidate match {
      case lp: Lambda =>
        val scala2FullDbFormLambda = if (lp.input.size == self.parameters.size) {
          List(lp.combine(self.parameters.map(_.ref)))
        } else {
          Nil
        }
        val scala3FullDbFormLambda = if (lp.input.size == selfNormalizedLambdaParamsSize) {
          val ps = self.parameters.collect { case FakeParam(pRef) => pRef }
          val selfPs = ps.sortBy(_.ref.asInstanceOf[SymName.LambdaParamName].index)
          List(lp.combine(selfPs))
        } else {
          Nil
        }
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

  private def isSelfRefinement(child: AbstractReference, candidate: AbstractReference): Boolean = candidate match {
    case Refinement(ref, _) => ref == child
    case _ => false
  }

  private def oneOfParameterizedParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
    val allParents = parameterizedParentsOf(child)
    // Self-refinement edges (child -> Refinement(child, decls)) only reduce subtyping obligations when the target
    // parent is itself a Refinement; skipping them for non-refinement targets prevents cyclic self-transitions.
    val parents = if (parent.isInstanceOf[Refinement]) {
      allParents
    } else {
      allParents.filterNot(isSelfRefinement(child, _))
    }
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
