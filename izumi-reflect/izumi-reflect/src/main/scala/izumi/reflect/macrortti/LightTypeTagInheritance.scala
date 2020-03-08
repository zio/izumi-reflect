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
import izumi.reflect.internal.fundamentals.platform.strings.IzString._
import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTagInheritance._
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable

object LightTypeTagInheritance {
  private[macrortti] final val tpeNothing = NameReference("scala.Nothing")
  private[macrortti] final val tpeAny = NameReference("scala.Any")
  private[macrortti] final val tpeAnyRef = NameReference("scala.AnyRef")
  private[macrortti] final val tpeObject = NameReference(classOf[Object].getName)

  final case class Ctx(params: List[LambdaParameter], logger: TrivialLogger) {
    def next(): Ctx = Ctx(params, logger.sub())
    def next(newparams: List[LambdaParameter]): Ctx = Ctx(newparams, logger.sub())
  }
}

final class LightTypeTagInheritance(self: LightTypeTag, other: LightTypeTag) {
  private[this] lazy val ib: ImmutableMultiMap[NameReference, NameReference] = LightTypeTag.mergeIDBs(self.idb, other.idb)
  private[this] lazy val bdb: ImmutableMultiMap[AbstractReference, AbstractReference] = LightTypeTag.mergeIDBs(self.basesdb, other.basesdb)

  def isChild(): Boolean = {
    val st = self.ref
    val ot = other.ref
    val logger = TrivialLogger.make[this.type](DebugProperties.`izumi.reflect.debug.macro.rtti`)

    logger.log(
      s"""⚙️ Inheritance check: $self vs $other
         |⚡️bases: ${bdb.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}
         |⚡️inheritance: ${ib.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}""".stripMargin)

    isChild(Ctx(List.empty, logger))(st, ot)
  }

  implicit class CtxExt(val ctx: Ctx) {
    def isChild(selfT0: LightTypeTagRef, thatT0: LightTypeTagRef): Boolean = LightTypeTagInheritance.this.isChild(ctx.next())(selfT0, thatT0)
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
      case (s: FullReference, t: FullReference) =>
        if (parentsOf(s.asName).contains(t)) {
          true
        } else {
          oneOfKnownParentsIsInheritedFrom(ctx)(s, t) || compareParameterizedRefs(ctx)(s, t)
        }
      case (s: FullReference, t: NameReference) =>
        oneOfKnownParentsIsInheritedFrom(ctx)(s, t)
      case (s: NameReference, t: FullReference) =>
        oneOfKnownParentsIsInheritedFrom(ctx)(s, t)
      case (s: NameReference, t: NameReference) =>
        val boundIsOk = t.boundaries match {
          case Boundaries.Defined(bottom, top) =>
            ctx.isChild(s, top) && ctx.isChild(bottom, s)
          case Boundaries.Empty =>
            true
        }

        any(
          all(boundIsOk, safeParentsOf(s).exists(p => ctx.isChild(p, thatT))),
          all(boundIsOk, parentsOf(s).exists(p => ctx.isChild(p, thatT))),
          all(boundIsOk, params.map(_.name).contains(t.ref.name)), // lambda parameter may accept anything
          s.boundaries match {
            case Boundaries.Defined(_, top) =>
              ctx.isChild(top, t)
            case Boundaries.Empty =>
              false
          }
        )

      case (_: AppliedNamedReference, t: Lambda) =>
        isChild(ctx.next(t.input))(selfT, t.output)
      case (s: Lambda, t: AppliedNamedReference) =>
        isChild(ctx.next(s.input))(s.output, t)
      case (s: Lambda, o: Lambda) =>
        s.input.size == o.input.size && isChild(ctx.next(s.normalizedParams.map(p => LambdaParameter(p.ref.name))))(s.normalizedOutput, o.normalizedOutput)
      case (s: IntersectionReference, t: IntersectionReference) =>
        // yeah, this shit is quadratic
        s.refs.forall {
          c =>
            t.refs.exists {
              p =>
                ctx.isChild(c, p)
            }
        }
      case (s: IntersectionReference, t: LightTypeTagRef) =>
        s.refs.exists(c => ctx.isChild(c, t))
      case (s: LightTypeTagRef, o: IntersectionReference) =>
        o.refs.forall(t => ctx.isChild(s, t))

      case (s: Refinement, t: Refinement) =>
        ctx.isChild(s.reference, t.reference) && t.decls.diff(s.decls).isEmpty
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfKnownParentsIsInheritedFrom(ctx)(s, t)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} $selfT <:< $thatT == $result")
    result
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
      val allParents = safeParentsOf(self)
      val moreParents = bdb.collect {
        case (l: Lambda, b) if isSame(l.output, self.asName) => b.collect {
          case l: Lambda if l.input.size == self.parameters.size => l
        }.map(l => l.combine(self.parameters.map(_.ref)))
      }.flatten
      ctx.logger.log(s"ℹ️ all parents of $self: $allParents ==> $moreParents")
      (allParents ++ moreParents)
        .exists {
          l =>
            val maybeParent = l
            ctx.isChild(maybeParent, that)
        }
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

  private def parentsOf(t: NameReference): Set[AppliedNamedReference] = {
    val out = mutable.HashSet[NameReference]()
    val tested = mutable.HashSet[NameReference]()
    parentsOf(t, out, tested)
    out.toSet
  }

  private def parentsOf(t: NameReference, out: mutable.HashSet[NameReference], tested: mutable.HashSet[NameReference]): Unit = {
    val direct = ib.get(t).toSet.flatten
    tested += t
    out ++= direct

    val nextNames = direct.map(_.asName)
    nextNames.diff(tested)
      .foreach {
        b =>
          parentsOf(b.asName, out, tested)
      }

  }

  private def safeParentsOf(t: AbstractReference): Seq[AbstractReference] = {
    bdb.get(t).toSeq.flatten
  }

  private def oneOfKnownParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
    safeParentsOf(child).exists(p => ctx.isChild(p, parent))
  }

}
