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
import izumi.reflect.internal.fundamentals.platform.language.unused
import izumi.reflect.macrortti.LightTypeTagRef._

object RuntimeAPI {

  def unpack(ref: AbstractReference): Set[NameReference] = {
    def unpackBoundaries(b: Boundaries): Set[NameReference] = {
      b match {
        case Boundaries.Defined(bottom, top) =>
          unpack(bottom) ++ unpack(top)
        case Boundaries.Empty =>
          Set.empty
      }
    }

    ref match {
      case Lambda(_, output) =>
        unpack(output)
      case reference: AppliedReference =>
        reference match {
          case reference: AppliedNamedReference =>
            reference match {
              case n: NameReference =>
                Set(n.copy(prefix = None, boundaries = Boundaries.Empty)) ++ n.prefix.toSet.flatMap(unpack) ++ unpackBoundaries(n.boundaries)
              case f: FullReference =>
                f.parameters.iterator.map(_.ref).flatMap(unpack).toSet ++ f.prefix.toSet.flatMap(unpack) + f.asName
            }
          case _: WildcardReference =>
            Set.empty
          case IntersectionReference(refs) =>
            refs.flatMap(unpack)
          case UnionReference(refs) =>
            refs.flatMap(unpack)
          case Refinement(reference, decls) =>
            unpack(reference) ++ decls.flatMap(
              d =>
                d match {
                  case RefinementDecl.Signature(_, input, output) =>
                    unpack(output) ++ input.flatMap(unpack)
                  case RefinementDecl.TypeMember(_, ref) =>
                    unpack(ref)
                }
            )
        }
    }
  }

  def applyLambda(lambda: Lambda, parameters: Seq[(String, AbstractReference)]): AbstractReference = {

    val paramMap = parameters.toMap

    val rewriter = new Rewriter(paramMap)
    val replaced = rewriter.replaceRefs(lambda.output)

    // fix for #82, #83
    // the old logic was ill, it was causing incorrect replacements when names from substitutions were clashing with replacement map
    // in fact that was a broken workaround for the lack of recursion in Rewriter

    //    val replaced = parameters.foldLeft(lambda.output) {
    //      case (acc, p) =>
    //        val rewriter = new Rewriter(Map(p))
    //        rewriter.replaceRefs(acc)
    //    }

    val newParams = lambda.input.filterNot(paramMap contains _.name)
    val res = if (newParams.isEmpty) {
      replaced
    } else {
      val out = Lambda(newParams, replaced)
      // assert(out.allArgumentsReferenced, s"bad lambda: $out, ${out.paramRefs}, ${out.referenced}")
      // such lambdas are legal: see "regression test: combine Const Lambda to TagK"
      out
    }

    res
  }

  final class Rewriter(rules: Map[String, AbstractReference]) {
    def complete(@unused context: AppliedNamedReference, ref: AbstractReference): AbstractReference = {
      ref
    }

    @nowarn("msg=view.filterKeys")
    def replaceRefs(reference: AbstractReference): AbstractReference = {
      reference match {
        case l: Lambda =>
          val bad = l.input.iterator.map(_.name).toSet
          val fixed = new Rewriter(rules.filterKeys(!bad(_)).toMap).replaceRefs(l.output)
          l.copy(output = fixed)

        case o: AppliedReference =>
          replaceApplied(o)
      }
    }

    def replacePrefix(prefix: Option[AppliedReference]): Option[AppliedReference] = {
      prefix.map(p => ensureApplied(p, replaceApplied(p)))
    }

    def replaceBoundaries(boundaries: Boundaries): Boundaries = {
      boundaries match {
        case Boundaries.Defined(bottom, top) =>
          Boundaries.Defined(replaceRefs(bottom), replaceRefs(top))
        case Boundaries.Empty =>
          boundaries
      }
    }

    private def replaceApplied(reference: AppliedReference): AbstractReference = {
      reference match {
        case IntersectionReference(refs) =>
          val replaced = refs.map(replaceApplied).map(r => ensureApplied(reference, r))
          maybeIntersection(replaced)
        case UnionReference(refs) =>
          val replaced = refs.map(replaceApplied).map(r => ensureApplied(reference, r))
          maybeUnion(replaced)
        case WildcardReference(boundaries) =>
          WildcardReference(replaceBoundaries(boundaries))
        case Refinement(base, decls) =>
          val rdecls = decls.map {
            case RefinementDecl.Signature(name, input, output) =>
              RefinementDecl.Signature(name, input.map(p => ensureApplied(reference, replaceRefs(p))), ensureApplied(reference, replaceRefs(output))): RefinementDecl
            case RefinementDecl.TypeMember(name, ref) =>
              RefinementDecl.TypeMember(name, replaceRefs(ref)): RefinementDecl
          }

          Refinement(ensureApplied(base, replaceApplied(base)), rdecls)
        case n: AppliedNamedReference =>
          replaceNamed(n)
      }
    }

    private def replaceNamed(reference: AppliedNamedReference): AbstractReference = {
      def returnFullRef(fixedRef: String, parameters: List[TypeParam], prefix: Option[AppliedReference]): FullReference = {
        val p = parameters.map {
          case TypeParam(pref, variance) =>
            TypeParam(replaceRefs(pref), variance)
        }
        FullReference(fixedRef, p, prefix)
      }

      reference match {
        case n @ NameReference(ref, boundaries, prefix) =>
          rules.get(ref.name) match {
            case Some(value) =>
              complete(n, value)
            case None =>
              NameReference(ref, replaceBoundaries(boundaries), replacePrefix(prefix))
          }

        case f @ FullReference(ref, parameters, prefix) =>
          rules.get(ref) match {
            case Some(value) =>
              complete(f, value) match {
                case out: Lambda =>
                  val refs = parameters.map(_.ref)
                  val res = replaceRefs(out.applySeq(refs)) // fix for #82, #83
                  res

                case n: NameReference =>
                  // we need this to support fakes only (see LightTypeTagRef#makeFakeParams)
                  returnFullRef(n.ref.name, parameters, prefix)

                case out =>
                  throw new IllegalStateException(s"Lambda expected for context-bound $f, but got $out")

              }
            case None =>
              returnFullRef(ref, parameters, prefix)
          }

      }
    }

    private def ensureApplied(context: AbstractReference, ref: AbstractReference): AppliedReference = {
      ref match {
        case reference: AppliedReference =>
          reference
        case o =>
          throw new IllegalStateException(s"Expected applied reference but got $o while processing $context")
      }
    }
  }

}
