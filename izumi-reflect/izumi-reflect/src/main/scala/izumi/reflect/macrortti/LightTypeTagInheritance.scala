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
  private[reflect] final val tpeAny = NameReference(SymTypeName("scala.Any"))
  private[reflect] final val tpeAnyRef = NameReference(SymTypeName("scala.AnyRef"))
  private[reflect] final val tpeMatchable = NameReference(SymTypeName("scala.Matchable"))
  private[reflect] final val tpeObject = NameReference(SymTypeName(classOf[Object].getName))

  private final case class Ctx(
    outerLambdaParams: List[LambdaParameter],
    paramNames: Set[String],
    outerDecls: Set[RefinementDecl.TypeMember],
    declNames: Set[String],
    logger: TrivialLogger,
    self: LightTypeTagInheritance
  ) {
    def next(): Ctx = Ctx(outerLambdaParams, paramNames, outerDecls, declNames, logger.sub(), self)
    def next(newparams: List[LambdaParameter]): Ctx = Ctx(newparams, newparams.iterator.map(_.name).toSet, outerDecls, declNames, logger.sub(), self)
    def next(newdecls: Set[RefinementDecl.TypeMember]): Ctx = Ctx(outerLambdaParams, paramNames, newdecls, newdecls.map(_.name), logger.sub(), self)
  }
  // https://github.com/lampepfl/dotty/issues/14013 // private implicit final class CtxExt(private val ctx: Ctx) extends AnyVal {
  private[LightTypeTagInheritance] implicit final class CtxExt(private val ctx: Ctx) extends AnyVal {
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

    isChild(Ctx(Nil, Set.empty, Set.empty, Set.empty, logger, this))(st, ot)
  }

  private def isChild(ctx: Ctx)(selfT: LightTypeTagRef, thatT: LightTypeTagRef): Boolean = {
    import ctx._
    logger.log(s"✴️ ️${selfT.repr} <:< ${thatT.repr}, context = ${ctx.outerLambdaParams}")

    val result = (selfT, thatT) match {
      case (s, t) if s == t =>
        true
      case (s, _) if s == tpeNothing =>
        true
      case (_, t) if t == tpeAny || t == tpeAnyRef || t == tpeObject =>
        // TODO: we may want to check that in case of anyref target type is not a primitve (though why?)
        true

      case (s: WildcardReference, t: WildcardReference) =>
        s.boundaries match {
          case Boundaries.Defined(_, top) =>
            compareBounds(ctx)(top, t.boundaries)
          case Boundaries.Empty =>
            t.boundaries match {
              case Boundaries.Defined(_, _) =>
                false
              case Boundaries.Empty =>
                true
            }
        }
      case (s: AppliedNamedReference, t: WildcardReference) =>
        compareBounds(ctx)(s, t.boundaries)
      case (s: Lambda, t: WildcardReference) =>
        isChild(ctx.next(s.input))(s.output, t)
      case (s: WildcardReference, t) =>
        s.boundaries match {
          case Boundaries.Defined(_, top) =>
            ctx.next().isChild(top, t)
          case Boundaries.Empty =>
            s == t
        }
      // parameterized type
      case (s: FullReference, t: FullReference) =>
        (oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
        || compareParameterizedRefs(ctx)(s, t))

      case (s: FullReference, t: NameReference) =>
        any(
          oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t),
          all(
            compareBounds(ctx)(s, t.boundaries),
            any(
//              outerLambdaParams.map(_.name).contains(t.ref.name), // lambda parameter may accept anything within bounds      // UNSOUND-LAMBDA-COMPARISON
              outerDecls.map(_.name).contains(t.ref.name) // refinement type decl may accept anything within bounds
            )
          )
        )

      case (s: NameReference, t: FullReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)

      // unparameterized type
      case (s: NameReference, t: NameReference) =>
        val boundIsOk = compareBounds(ctx)(s, t.boundaries)

        any(
          all(boundIsOk, parameterizedParentsOf(s).exists(ctx.isChild(_, t))),
          all(boundIsOk, unparameterizedParentsOf(s).exists(ctx.isChild(_, t))),
//          all(boundIsOk, outerLambdaParams.map(_.name).contains(t.ref.name)), // lambda parameter may accept anything within bounds       // UNSOUND-LAMBDA-COMPARISON
          all(boundIsOk, outerDecls.map(_.name).contains(t.ref.name)), // refinement decl may accept anything within bounds
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
        (s.input.size == o.input.size
        && isChild(ctx.next(s.normalizedParams.map(p => LambdaParameter(p.ref.name))))(s.normalizedOutput, o.normalizedOutput))

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
        (ctx.isChild(s.reference, t.reference)
        && compareDecls(ctx.next(t.decls.collect { case tm: RefinementDecl.TypeMember => tm }))(s.decls, t.decls))
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} ${selfT.repr} <:< ${thatT.repr} == $result")
    result
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
    case (RefinementDecl.TypeMember(ln, lref), RefinementDecl.TypeMember(rn, rref)) =>
      ln == rn && ctx.isChild(lref, rref)
    case (RefinementDecl.Signature(ln, lins, lout), RefinementDecl.Signature(rn, rins, rout)) =>
      (ln == rn
      && lins.iterator.zip(rins.iterator).forall { case (l, r) => ctx.isChild(r, l) } // contravariant
      && ctx.isChild(lout, rout)) // covariant
    case _ =>
      false
  }

  private def compareParameterizedRefs(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {
    def parameterShapeCompatible: Boolean = {
      self.parameters.zip(that.parameters).forall {
        case (ps, pt) =>
          pt.variance match {
            case Variance.Invariant =>
              pt.ref match {
                case wc: LightTypeTagRef.WildcardReference =>
                  compareBounds(ctx)(ps.ref, wc.boundaries)
                case _ =>
                  ps.ref == pt.ref
              }
            case Variance.Contravariant =>
              pt.ref match {
                case wc: LightTypeTagRef.WildcardReference =>
                  wc.boundaries match {
                    case Boundaries.Defined(bottom, _) =>
                      ctx.isChild(bottom, ps.ref)

                    case Boundaries.Empty =>
                      true
                  }
                case _ =>
                  ctx.isChild(pt.ref, ps.ref)
              }

            case Variance.Covariant =>
              pt.ref match {
                case wc: LightTypeTagRef.WildcardReference =>
                  wc.boundaries match {
                    case Boundaries.Defined(_, top) =>
                      ctx.isChild(ps.ref, top)

                    case Boundaries.Empty =>
                      true
                  }
                case _ =>
                  ctx.isChild(ps.ref, pt.ref)
              }
          }
      }
    }

    def sameArity: Boolean = {
      self.parameters.size == that.parameters.size
    }

    ctx
      .logger.log(
        s"⚠️ comparing parameterized references, ${self.repr} <:< ${that.repr}, context = ${ctx.outerLambdaParams}; sameArity = $sameArity, shapeOk = $parameterShapeCompatible"
      )

    if (self.asName == that.asName) {
      sameArity && parameterShapeCompatible
    } else if (ctx.isChild(self.asName, that.asName)) {
      val allParents = parameterizedParentsOf(self)
      val inferredLambdaParents = basesdb.collect {
        case (l: Lambda, b) if isSame(l.output, self.asName) =>
          b.collect {
            case l: Lambda if l.input.size == self.parameters.size => l
          }.map(l => l.combine(self.parameters.map(_.ref)))
      }.flatten
      ctx.logger.log(s"ℹ️ all parents of ${self.repr}: baseDbParents=${allParents.map(_.repr)} ==> inferredLambdaParents=${inferredLambdaParents.map(_.repr)}")
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
    basesdb.getOrElse(t, Set.empty)
  }

  private def oneOfParameterizedParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
    ctx.logger.log(s"Looking up parameterized parents of $child => ${parameterizedParentsOf(child)}")
//    ctx.logger.log(s"Checking if ${parameterizedParentsOf(child)} has a parent of $parent")
    parameterizedParentsOf(child).exists(ctx.isChild(_, parent))
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
