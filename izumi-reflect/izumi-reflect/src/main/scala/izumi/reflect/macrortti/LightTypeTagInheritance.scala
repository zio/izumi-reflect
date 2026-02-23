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

    val result = (selfT, thatT) match {
      case (s, t) if s == t =>
        true
      case (s, _) if s == tpeNothing =>
        true
      case (s, _) if s == tpeNull =>
        // TODO: we may want to check that in case of anyref target type is not a primitve (though why?)
        true
      case (_, t) if t == tpeAny || t == tpeAnyRef || t == tpeObject || t == tpeMatchable =>
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
        isChild(ctx.next())(s.output, t)
      case (s: WildcardReference, t) =>
        s.boundaries match {
          case Boundaries.Defined(_, top) =>
            ctx.next().isChild(top, t)
          case Boundaries.Empty =>
            s == t
        }
      // parameterized type
      case (s: FullReference, t: FullReference) =>
        compareParameterizedRefs(ctx)(s, t)

      case (s: FullReference, t: NameReference) =>
        any(
          oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t),
          all(
            compareBounds(ctx)(s, t.boundaries),
            any(
              oneOfUnparameterizedParentsIsInheritedFrom(ctx)(s.asName, t), // constructor inherits from rhs, where rhs is an unparameterized type
//              outerLambdaParams.map(_.name).contains(t.ref.name), // lambda parameter may accept anything within bounds      // UNSOUND-LAMBDA-COMPARISON
//              t.ref.maybeName.exists(outerDecls.map(_.name).contains), // refinement type decl may accept anything within bounds,
              isInBoundsOfAnEqualBoundedAbstractType(s, t) // equal-bounded abstract type
            )
          )
        )

      case (s: NameReference, t: FullReference) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)

      // unparameterized type
      case (s: NameReference, t: NameReference) =>
        val boundIsOk = compareBounds(ctx)(s, t.boundaries)
        any(
          boundIsOk && any(
            oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t),
            oneOfUnparameterizedParentsIsInheritedFrom(ctx)(s, t),
            // outerLambdaParams.map(_.name).contains(t.ref.name), // lambda parameter may accept anything within bounds       // UNSOUND-LAMBDA-COMPARISON
//            t.ref.maybeName.exists(outerDecls.map(_.name).contains), // refinement decl may accept anything within bounds
            isInBoundsOfAnEqualBoundedAbstractType(s, t) // equal-bounded abstract type
          ),
          s.boundaries match {
            case Boundaries.Defined(_, sUp) =>
              ctx.isChild(sUp, t)
            case Boundaries.Empty =>
              false
          }
        )

      // lambdas
      case (s: AppliedNamedReference, t: Lambda) =>
        isChild(ctx.next())(s, t.output)
      case (s: Lambda, t: AppliedNamedReference) =>
        isChild(ctx.next())(s.output, t)
      case (s: Lambda, o: Lambda) =>
        s.input.size == o.input.size && {
          val sBoundsMap = collectLambdaParamBoundaries(s.output, s.input.toSet)
          val oBoundsMap = collectLambdaParamBoundaries(o.output, o.input.toSet)
          val boundsOk = s.input.zip(o.input).forall {
            case (sp, op) =>
              val sBounds = sBoundsMap.getOrElse(sp, Boundaries.Empty)
              val oBounds = oBoundsMap.getOrElse(op, Boundaries.Empty)
              compareLambdaParamBounds(ctx)(sBounds, oBounds)
          }
          boundsOk && {
            val sOut = stripLambdaParamBoundaries(s.normalizedOutput, s.input.toSet)
            val oOut = stripLambdaParamBoundaries(o.normalizedOutput, o.input.toSet)
            isChild(ctx.next())(sOut, oOut)
          }
        }

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
        ctx.isChild(s.reference, t.reference) && compareDecls(ctx.next())(s.decls, t.decls)
      case (s: Refinement, t: LightTypeTagRef) =>
        ctx.isChild(s.reference, t)
      case (s: AbstractReference, t: Refinement) =>
        oneOfParameterizedParentsIsInheritedFrom(ctx)(s, t)
    }
    logger.log(s"${if (result) "✅" else "⛔️"} isChild: `${selfT.repr}` <:< `${thatT.repr}` == $result")
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

  /** For lambda subtyping `[A >: L1 <: U1] => T1 <: [B >: L2 <: U2] => T2`,
    * the child's bounds must be at least as wide as the parent's: L1 <: L2 and U2 <: U1.
    */
  private def compareLambdaParamBounds(ctx: Ctx)(childBounds: Boundaries, parentBounds: Boundaries): Boolean = {
    (childBounds, parentBounds) match {
      case (_, Boundaries.Empty) =>
        true
      case (Boundaries.Empty, Boundaries.Defined(_, _)) =>
        true // unbounded child accepts everything
      case (Boundaries.Defined(cLo, cHi), Boundaries.Defined(pLo, pHi)) =>
        ctx.isChild(pLo, cLo) && ctx.isChild(cHi, pHi)
    }
  }

  private def collectLambdaParamBoundaries(
    ref: AbstractReference,
    params: Set[SymName.LambdaParamName]
  ): Map[SymName.LambdaParamName, Boundaries] = {
    val result = mutable.HashMap.empty[SymName.LambdaParamName, Boundaries]

    def collectBoundaries(b: Boundaries): Unit = b match {
      case Boundaries.Defined(bottom, top) => visit(bottom); visit(top)
      case Boundaries.Empty =>
    }

    def visit(r: AbstractReference): Unit = r match {
      case NameReference(lpn: SymName.LambdaParamName, boundaries, prefix) =>
        if (params.contains(lpn) && !result.contains(lpn) && boundaries != Boundaries.Empty) {
          result(lpn) = boundaries
        }
        collectBoundaries(boundaries)
        prefix.foreach(visit)
      case NameReference(_, boundaries, prefix) =>
        collectBoundaries(boundaries)
        prefix.foreach(visit)
      case FullReference(_, parameters, prefix) =>
        parameters.foreach(p => visit(p.ref))
        prefix.foreach(visit)
      case Lambda(_, output) =>
        visit(output)
      case IntersectionReference(refs) =>
        refs.foreach(visit)
      case UnionReference(refs) =>
        refs.foreach(visit)
      case WildcardReference(boundaries) =>
        collectBoundaries(boundaries)
      case Refinement(base, decls) =>
        visit(base)
        decls.foreach {
          case RefinementDecl.Signature(_, input, output) => input.foreach(visit); visit(output)
          case RefinementDecl.TypeMember(_, r) => visit(r)
        }
    }

    visit(ref)
    result.toMap
  }

  private def stripLambdaParamBoundaries(
    ref: AbstractReference,
    params: Set[SymName.LambdaParamName]
  ): AbstractReference = ref match {
    case NameReference(lpn: SymName.LambdaParamName, _, prefix) if params.contains(lpn) =>
      NameReference(lpn, Boundaries.Empty, prefix.map(p => stripLambdaParamBoundaries(p, params).asInstanceOf[AppliedReference]))
    case NameReference(r, boundaries, prefix) =>
      NameReference(r, stripBoundaries(boundaries, params), prefix.map(p => stripLambdaParamBoundaries(p, params).asInstanceOf[AppliedReference]))
    case FullReference(symName, parameters, prefix) =>
      FullReference(
        symName,
        parameters.map(p => TypeParam(stripLambdaParamBoundaries(p.ref, params), p.variance)),
        prefix.map(p => stripLambdaParamBoundaries(p, params).asInstanceOf[AppliedReference])
      )
    case Lambda(input, output) =>
      new Lambda(input, stripLambdaParamBoundaries(output, params))
    case IntersectionReference(refs) =>
      IntersectionReference(refs.map(r => stripLambdaParamBoundaries(r, params).asInstanceOf[AppliedReferenceExceptIntersection]))
    case UnionReference(refs) =>
      UnionReference(refs.map(r => stripLambdaParamBoundaries(r, params).asInstanceOf[AppliedReferenceExceptUnion]))
    case WildcardReference(boundaries) =>
      WildcardReference(stripBoundaries(boundaries, params))
    case Refinement(base, decls) =>
      Refinement(
        stripLambdaParamBoundaries(base, params).asInstanceOf[AppliedReference],
        decls.map {
          case RefinementDecl.Signature(name, input, output) =>
            RefinementDecl.Signature(
              name,
              input.map(i => stripLambdaParamBoundaries(i, params).asInstanceOf[AppliedReference]),
              stripLambdaParamBoundaries(output, params).asInstanceOf[AppliedReference]
            ): RefinementDecl
          case RefinementDecl.TypeMember(name, r) =>
            RefinementDecl.TypeMember(name, stripLambdaParamBoundaries(r, params)): RefinementDecl
        }
      )
  }

  private def stripBoundaries(boundaries: Boundaries, params: Set[SymName.LambdaParamName]): Boundaries = boundaries match {
    case Boundaries.Defined(bottom, top) =>
      Boundaries.Defined(stripLambdaParamBoundaries(bottom, params), stripLambdaParamBoundaries(top, params))
    case Boundaries.Empty =>
      Boundaries.Empty
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
      ln == rn && compareBounds(ctx)(lref, rBounds)
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

  private def compareParameterizedRefs(ctx: Ctx)(self: FullReference, that: FullReference): Boolean = {

    def parametersConform: Boolean = {
      self.parameters.zipAll(that.parameters, null, null).forall {
        case (ps, pt) =>
          (ps ne null) && (pt ne null) && (pt.variance match {
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
          })
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
      val appliedLambdaParents = basesdb
        .iterator.collect {
          case (l: Lambda, lparents) if isSameNamedRef(l.output, selfNameRef) =>
            lparents.flatMap {
              case lp: Lambda =>
                val scala2FullDbFormLambda = {
                  if (lp.input.size == self.parameters.size) {
                    List(lp.combine(self.parameters.map(_.ref)))
                  } else {
                    Nil
                  }
                }

                val scala3FullDbFormLambda = {
                  // 0 unless we're in a lambda vs. lambda comparison (s.normalizedOutput <:< t.normalizedOutput)
                  val selfNormalizedLambdaParamsSize = self.parameters.count(p => isFakeParam(p.ref))
                  if (lp.input.size == selfNormalizedLambdaParamsSize) {
                    val ps = self.parameters.collect { case FakeParam(pRef) => pRef }
                    val selfPs = ps.sortBy(_.ref.asInstanceOf[SymName.LambdaParamName].index)
                    val res = lp.combine(selfPs)
                    List(res)
                  } else Nil
                }

                scala2FullDbFormLambda ++ scala3FullDbFormLambda
              case _ =>
                Nil
            }
        }.flatten.toList
      ctx.logger.log {
        s"ℹ️ checking applied lambda parents of self=`${self.repr}`: parents=${appliedLambdaParents.map(_.repr)} <:< that=`${that.repr}`"
      }
      val normalizedThat = normalizeLambdas(that)
      appliedLambdaParents.exists {
        p =>
          ctx.isChild(p, that) || ctx.isChild(normalizeLambdas(p), normalizedThat)
      }
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

  private def normalizeLambdas(reference: AbstractReference): AbstractReference = {
    reference match {
      case l: Lambda =>
        l.normalize()
      case IntersectionReference(refs) =>
        IntersectionReference(refs.map(normalizeLambdas).map {
          case r: AppliedReferenceExceptIntersection => r
          case other => throw new IllegalStateException(s"Expected AppliedReferenceExceptIntersection, got: $other")
        })
      case UnionReference(refs) =>
        UnionReference(refs.map(normalizeLambdas).map {
          case r: AppliedReferenceExceptUnion => r
          case other => throw new IllegalStateException(s"Expected AppliedReferenceExceptUnion, got: $other")
        })
      case WildcardReference(boundaries) =>
        WildcardReference(
          boundaries match {
            case Boundaries.Defined(bottom, top) =>
              Boundaries.Defined(normalizeLambdas(bottom), normalizeLambdas(top))
            case Boundaries.Empty =>
              Boundaries.Empty
          }
        )
      case Refinement(base, decls) =>
        val normalizedBase = normalizeLambdas(base) match {
          case a: AppliedReference => a
          case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
        }
        val normalizedDecls = decls.map {
          case RefinementDecl.Signature(name, input, output) =>
            RefinementDecl.Signature(
              name,
              input.map(normalizeLambdas).map {
                case a: AppliedReference => a
                case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
              },
              normalizeLambdas(output) match {
                case a: AppliedReference => a
                case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
              }
            ): RefinementDecl
          case RefinementDecl.TypeMember(name, ref) =>
            RefinementDecl.TypeMember(name, normalizeLambdas(ref)): RefinementDecl
        }
        Refinement(normalizedBase, normalizedDecls)
      case NameReference(ref, boundaries, prefix) =>
        NameReference(
          ref,
          boundaries match {
            case Boundaries.Defined(bottom, top) =>
              Boundaries.Defined(normalizeLambdas(bottom), normalizeLambdas(top))
            case Boundaries.Empty =>
              Boundaries.Empty
          },
          prefix.map(normalizeLambdas).map {
            case a: AppliedReference => a
            case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
          }
        )
      case FullReference(symName, parameters, prefix) =>
        FullReference(
          symName,
          parameters.map(p => TypeParam(normalizeLambdas(p.ref), p.variance)),
          prefix.map(normalizeLambdas).map {
            case a: AppliedReference => a
            case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
          }
        )
    }
  }

  private def parameterizedParentsOf(t: AbstractReference): Set[AbstractReference] = {
    val basesDbParents = basesdb.getOrElse(t, Set.empty)
    val templateParents = t match {
      case actual: FullReference =>
        resolveTemplateParameterizedParents(actual)
      case _ =>
        Set.empty[AbstractReference]
    }
    val allParents = basesDbParents ++ templateParents
    val withBoundaryParents = t match {
      case NameReference(_, b: Boundaries.Defined, _) =>
        allParents + b.top
      case WildcardReference(b: Boundaries.Defined) =>
        allParents + b.top
      case _ =>
        allParents
    }
    withBoundaryParents
  }

  private def resolveTemplateParameterizedParents(actual: FullReference): Set[AbstractReference] = {
    basesdb.iterator.collect {
      case (template: FullReference, templateParents) =>
        buildTemplateSubstitution(template, actual).map {
          substitution =>
            val rewriter = new RuntimeAPI.Rewriter(substitution)
            templateParents.map(rewriter.replaceRefs)
        }
    }.flatten.flatten.toSet
  }

  private def buildTemplateSubstitution(
    template: FullReference,
    actual: FullReference
  ): Option[Map[SymName.LambdaParamName, AbstractReference]] = {
    if (template.asName != actual.asName || template.parameters.size != actual.parameters.size) {
      None
    } else {
      template.parameters.zip(actual.parameters).foldLeft(Option(Map.empty[SymName.LambdaParamName, AbstractReference])) {
        case (None, _) =>
          None
        case (Some(subst), (templateParam, actualParam)) =>
          templateParam.ref match {
            case NameReference(lambdaParam: SymName.LambdaParamName, Boundaries.Empty, None) =>
              subst.get(lambdaParam) match {
                case Some(existing) =>
                  if (existing == actualParam.ref) Some(subst) else None
                case None =>
                  Some(subst.updated(lambdaParam, actualParam.ref))
              }
            case otherRef =>
              if (otherRef == actualParam.ref) Some(subst) else None
          }
      }
    }
  }

  private def oneOfParameterizedParentsIsInheritedFrom(ctx: Ctx)(child: AbstractReference, parent: AbstractReference): Boolean = {
//    ctx.logger.log(s"Checking if ${parameterizedParentsOf(child)} has a parent of $parent")
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
