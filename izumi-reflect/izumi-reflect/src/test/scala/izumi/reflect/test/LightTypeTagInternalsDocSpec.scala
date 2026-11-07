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

package izumi.reflect.test

import izumi.reflect.macrortti.LightTypeTagRef._
import izumi.reflect.macrortti._

object LightTypeTagInternalsDocSpec {
  trait StructuralCarrier {
    def count: Int
  }

  final class NominalOnlyCarrier {
    def count: Int = 42
  }
}

class LightTypeTagInternalsDocSpec extends TagAssertions {
  import LightTypeTagInternalsDocSpec._

  "LightTypeTag internals documented in docs/internals.md" should {

    "maintain both parameterized basesdb and nominal idb entries" in {
      val listIntTag = LTT[List[Int]]
      val unpacked = LightTypeTagUnpacker(listIntTag)

      // Parameterized and higher-kinded ancestor mappings reside in basesdb,
      // while unparameterized nominal ancestor closures reside in idb.
      val hasLambdaBaseEntry = unpacked.bases.keys.exists(_.isInstanceOf[Lambda])
      val hasNominalAncestors = unpacked.inheritance.values.exists(_.nonEmpty)

      assert(hasLambdaBaseEntry)
      assert(hasNominalAncestors)
    }

    "normalize alpha-equivalent Lambdas via De Bruijn fake parameters" in {
      val intSym = SymName.SymTypeName("scala.Int")
      val eitherSym = SymName.SymTypeName("scala.util.Either")

      // Construct two identical binary type constructors whose bound parameter
      // symbols originate from distinct macro nesting depths (0 vs -3).
      val paramAtDepthZero = SymName.LambdaParamName(0, depth = 0, arity = 1)
      val paramAtTagMacroDepth = SymName.LambdaParamName(0, depth = LambdaConstants.tagMacro, arity = 1)

      val lambdaDepthZero = buildRightBiasedEitherLambda(eitherSym, intSym -> paramAtDepthZero)
      val lambdaTagMacro = buildRightBiasedEitherLambda(eitherSym, intSym -> paramAtTagMacroDepth)

      assert(lambdaDepthZero == lambdaTagMacro)
      assert(lambdaDepthZero.hashCode() == lambdaTagMacro.hashCode())
      assert(lambdaDepthZero.normalizedOutput == lambdaTagMacro.normalizedOutput)
    }

    "retain unapplied base lambdas in basesdb after runtime combine" in {
      val listCtorTag = `LTT[_]`[List]
      val appliedListInt = listCtorTag.combine(LTT[Int])
      val unpackedBases = LightTypeTagUnpacker(appliedListInt).bases

      // Retaining unapplied Lambda keys in basesdb after combination is required
      // so compareParameterizedRefs can synthesize appliedLambdaParents during <:< checks.
      val retainsUnappliedLambdaKeys = unpackedBases.keys.exists(_.isInstanceOf[Lambda])
      assert(retainsUnappliedLambdaKeys)
      assertChild(appliedListInt, LTT[Seq[AnyVal]])
    }

    "round-trip binary BooPickle serialization across current and pre-2.3.0 format versions" in {
      val original = LTT[Either[String, List[Int]]]
      val serialized = original.serialize()

      val parsedCurrent = LightTypeTag.parse[Either[String, List[Int]]](
        serialized.hash,
        serialized.ref,
        serialized.databases,
        LightTypeTag.currentBinaryFormatVersion
      )
      val parsedLegacy = LightTypeTag.parse[Either[String, List[Int]]](
        serialized.hash,
        serialized.ref,
        serialized.databases,
        version = 29
      )

      assert(serialized.version == LightTypeTag.currentBinaryFormatVersion)
      assertSameStrict(parsedCurrent, original)
      assertSameStrict(parsedLegacy, original)
      assert(parsedCurrent.hashCode() == parsedLegacy.hashCode())
    }

    "exhibit the documented soundness trade-offs for top/bottom types and structural refinements" in {
      // Null and Nothing are treated as universal bottom types, and AnyRef / Object
      // are treated as universal top types even for AnyVal primitives.
      assertChild(LTT[Null], LTT[Int])
      assertChild(LTT[Int], LTT[AnyRef])
      assertChild(LTT[Int], LTT[Object])

      // Concrete classes do not serialize their method symbol tables into Refinement
      // declarations unless explicitly typed as a structural refinement.
      val structuralTarget = LTT[{ def count: Int }]
      assertChild(LTT[StructuralCarrier { def count: Int }], structuralTarget)
      assertNotChild(LTT[NominalOnlyCarrier], structuralTarget)
    }
  }

  private def buildRightBiasedEitherLambda(
    eitherSym: SymName,
    leftAndRightSyms: (SymName, SymName.LambdaParamName)
  ): Lambda = {
    val (leftSym, rightParam) = leftAndRightSyms
    val leftArg = TypeParam(NameReference(leftSym), Variance.Covariant)
    val rightArg = TypeParam(NameReference(rightParam), Variance.Covariant)
    Lambda(
      List(rightParam),
      FullReference(eitherSym, List(leftArg, rightArg))
    )
  }
}
