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

import scala.annotation.nowarn
import izumi.reflect.internal.fundamentals.collections.ImmutableMultiMap
import izumi.reflect.internal.fundamentals.platform.basics.IzBoolean.*
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config
import izumi.reflect.internal.fundamentals.platform.strings.IzString.*
import izumi.reflect.macrortti.LightTypeTagInheritance.*
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.collection.mutable

object LightTypeTagInheritance {
  private[macrortti] final val tpeNothing = NameReference("scala.Nothing")
  private[macrortti] final val tpeAny = NameReference("scala.Any")
  private[macrortti] final val tpeAnyRef = NameReference("scala.AnyRef")
  private[macrortti] final val tpeObject = NameReference(classOf[Object].getName)

  private final case class Ctx(
    params: List[LambdaParameter],
    paramNames: Set[String],
    decls: Set[RefinementDecl],
    declNames: Set[String],
    logger: TrivialLogger,
    self: LightTypeTagInheritance
  ) {
    def next(): Ctx = Ctx(params, paramNames, decls, declNames, logger.sub(), self)
    def next(newparams: List[LambdaParameter]): Ctx = Ctx(newparams, newparams.iterator.map(_.name).toSet, decls, declNames, logger.sub(), self)
    def next(newdecls: Set[RefinementDecl]): Ctx = Ctx(params, paramNames, newdecls, newdecls.map(_.name), logger.sub(), self)
  }
  private implicit final class CtxExt(private val ctx: Ctx) extends AnyVal {
    def isChild(selfT0: LightTypeTagRef, thatT0: LightTypeTagRef): Boolean = ctx.self.isChild(ctx.next())(selfT0, thatT0)
  }
}

final class LightTypeTagInheritance(self: LightTypeTag, other: LightTypeTag) {
  private[this] lazy val ib: ImmutableMultiMap[NameReference, NameReference] = LightTypeTag.mergeIDBs(self.idb, other.idb)
  private[this] lazy val bdb: ImmutableMultiMap[AbstractReference, AbstractReference] = LightTypeTag.mergeIDBs(self.basesdb, other.basesdb)

  @nowarn("msg=view.mapValues")
  def isChild(): Boolean = {
    val st = self.ref
    val ot = other.ref
    val logger = TrivialLogger.make[this.type](config = Config.console)

    logger.log(s"""⚙️ Inheritance check: $self vs $other
                  |⚡️bases: ${bdb.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}
                  |⚡️inheritance: ${ib.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}""".stripMargin)

    isChild(Ctx(List.empty, Set.empty, Set.empty, Set.empty, logger, this))(st, ot)
  }

  private def isChild(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    import ctx._
    logger.log(s"✴️ ️$selfT <:< $thatT, context = ${ctx.params}")

    val result = (selfT, thatT) match {
      case (s, t) if s == t =>
        true
      case (s, _) if s == tpeNothing =>
        true
      case (_, t) if t == tpeAny || t == tpeAnyRef || t == tpeObject =>
        // TODO: we may want to check that in case of anyref target type is not a primitve (though why?)
        true

      // parameterized type
      case (s: FullReference, t: FullReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t) ||
        compareParameterizedRefs(ctx)(s, t)

      case (s: FullReference, t: NameReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t) || {
          val boundIsOk = compareBounds(ctx)(s, t)

          boundIsOk && (
            params.map(_.name).contains(t.ref.name) || // lambda parameter may accept anything within bounds
            decls.map(_.name).contains(t.ref.name) // refinement decl may accept anything within bounds
          )
        }

      case (s: NameReference, t: FullReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)

      // unparameterized type
      case (s: NameReference, t: NameReference) =>
        val boundIsOk = compareBounds(ctx)(s, t)

        any(
          all(boundIsOk, parameterizedParentsOf(s).exists(ctx.isChild(_, t))),
          all(boundIsOk, unparameterizedParentsOf(s).exists(ctx.isChild(_, t))),
          all(boundIsOk, params.map(_.name).contains(t.ref.name)), // lambda parameter may accept anything within bounds
          all(boundIsOk, decls.map(_.name).contains(t.ref.name)), // refinement decl may accept anything within bounds
          s.boundaries match {
            case Boundaries.Defined(_, sUp) =>
              ctx.isChild(sUp, t)
            case Boundaries.Empty =>
              false
          }
        )

      // lambdas
      case (s: AppliedNamedReference, t: Lambda) =>
        isChild(ctx.next(t.input))(s, t.output)
      case (s: Lambda, t: AppliedNamedReference) =>
        isChild(ctx.next(s.input))(s.output, t)
      case (s: Lambda, o: Lambda) =>
        s.input.size == o.input.size &&
        isChild(ctx.next(s.normalizedParams.map(p => LambdaParameter(p.ref.name))))(s.normalizedOutput, o.normalizedOutput)

      // intersections
      case (s: IntersectionReference, t: IntersectionReference) =>
        // yeah, this shit is quadratic
        t.refs.forall {
          p =>
            s.refs.exists {
              c =>
                ctx.isChild(c, p)
            }
        }
      case (s: IntersectionReference, t: LightTypeTagRef) =>
        s.refs.exists(c => ctx.isChild(c, t))
      case (s: LightTypeTagRef, o: IntersectionReference) =>
        o.refs.forall(t => ctx.isChild(s, t))

      // unions
      case (s: UnionReference, t: UnionReference) =>
        // yeah, this shit is quadratic
        s.refs.forall {
          c =>
            t.refs.exists {
              p =>
                ctx.isChild(c, p)
            }
        }
      case (s: UnionReference, t: LightTypeTagRef) =>
        s.refs.forall(c => ctx.isChild(c, t))
      case (s: LightTypeTagRef, o: UnionReference) =>
        o.refs.exists(t => ctx.isChild(s, t))

      // refinements
      case (s: Refinement, t: Refinement) =>
        ctx.isChild(s.reference, t.reference) &&
        compareDecls(ctx.next(t.decls))(s.decls, t.decls)
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} $selfT <:< $thatT == $result")
    result
  }

  private def compareBounds(ctx: Ctx)(s: AppliedNamedReference, t: NameReference): Boolean = {
    t.boundaries match {
      case Boundaries.Defined(tLow, tUp) =>
        ctx.isChild(s, tUp) &&
        ctx.isChild(tLow, s)
      case Boundaries.Empty =>
        true
    }
  }

  private def compareDecls(ctx: Ctx)(sDecls: Set[RefinementDecl], tDecls: Set[RefinementDecl]): Boolean = {
    val s = sDecls.groupBy(_.name)
    // for every decl on the right there's a <: decl on the left
    tDecls.forall {
      r =>
        val ls = s.get(r.name).toSet.flatten
        ls.exists(compareDecl(ctx)(_, r))
    }
  }

  private def compareDecl(ctx: Ctx)(s: RefinementDecl, t: RefinementDecl): Boolean = (s, t) match {
    case (RefinementDecl.TypeMember(ln, lref), RefinementDecl.TypeMember(rn, rref)) =>
      ln == rn && ctx.isChild(lref, rref)
    case (RefinementDecl.Signature(ln, lins, lout), RefinementDecl.Signature(rn, rins, rout)) =>
      ln == rn &&
      lins.iterator.zip(rins).forall { case (l, r) => ctx.isChild(r, l) } // contravariant
      ctx.isChild(lout, rout) // covariant
    case _ =>
      false
  }

  private def compareParameterizedRefs(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {
    def parameterShapeCompatible: Boolean = {
      self.parameters.zip(that.parameters).forall {
        case (ps, pt) =>
          pt.variance match {
            case Variance.Invariant =>
              ps.ref == pt.ref
            case Variance.Contravariant =>
              ctx.isChild(pt.ref, ps.ref)
            case Variance.Covariant =>
              ctx.isChild(ps.ref, pt.ref)
          }
      }
    }

    def sameArity: Boolean = {
      self.parameters.size == that.parameters.size
    }

    ctx.logger.log(s"⚠️ comparing parameterized references, $self <:< $that, context = ${ctx.params}; sameArity = $sameArity, shapeOk = $parameterShapeCompatible")

    if (self.asName == that.asName) {
      sameArity && parameterShapeCompatible
    } else if (ctx.isChild(self.asName, that.asName)) {
      val allParents = parameterizedParentsOf(self)
      val inferredLambdaParents = bdb.collect {
        case (l: Lambda, b) if isSame(l.output, self.asName) =>
          b.collect {
            case l: Lambda if l.input.size == self.parameters.size => l
          }.map(l => l.combine(self.parameters.map(_.ref)))
      }.flatten
      ctx.logger.log(s"ℹ️ all parents of $self: baseDbParents=$allParents ==> inferredLambdaParents=$inferredLambdaParents")
      (allParents.iterator ++ inferredLambdaParents)
        .exists(ctx.isChild(_, that))
    } else {
      false
    }
  }

  private def isSame(a: AbstractReference, b: AbstractReference): Boolean = {
    (a, b) match {
      case (an: AppliedNamedReference, ab: AppliedNamedReference) =>
        an.asName == ab.asName
      case _ =>
        false
    }
  }

  private def parameterizedParentsOf(t: AbstractReference): Set[AbstractReference] = {
    bdb.getOrElse(t, Set.empty)
  }

  private def oneOfParameterizedParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
    parameterizedParentsOf(child).exists(ctx.isChild(_, parent))
  }

  private def unparameterizedParentsOf(t: NameReference): mutable.HashSet[NameReference] = {
    def parentsOf(t: NameReference, out: mutable.HashSet[NameReference], tested: mutable.HashSet[NameReference]): Unit = {
      val direct = ib.get(t).toSet.flatten
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
