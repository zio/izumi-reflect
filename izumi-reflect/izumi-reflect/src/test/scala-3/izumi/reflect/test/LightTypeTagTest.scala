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

import izumi.reflect.Tag
import izumi.reflect.macrortti.*
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, Boundaries, Lambda}

import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

class LightTypeTagTest extends SharedLightTypeTagTest {

  import TestModel._

  "lightweight type tags (Dotty)" should {

    "tautological intersections with Matchable are discarded from internal structure (Scala 3 specific, Matchable)" in {
      assertSameStrict(LTT[Matchable with Option[String]], LTT[Option[String]])
      assertDebugSame(LTT[Matchable with Option[String]], LTT[Option[String]])
    }

    "tautological intersections with Matchable are discarded from internal structure (Scala 3 specific, Matchable) (Tag)" in {
      assertSameStrict(Tag[Matchable with Option[String]].tag, LTT[Option[String]])
      assertDebugSame(Tag[Matchable with Option[String]].tag, LTT[Option[String]])
    }

    "tautological unions with Any/AnyRef/Matchable/Object are discarded from internal structure (Scala 3 specific, Matchable)" in {
      assertSameStrict(LTT[Any | Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Any])
      assertDebugSame(LTT[Any | Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Any])

      assertSameStrict(LTT[Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Matchable])
      assertDebugSame(LTT[Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Matchable])

      assertSameStrict(LTT[AnyRef | Object | Option[String] | Nothing], LTT[AnyRef])
      assertDebugSame(LTT[AnyRef | Object | Option[String] | Nothing], LTT[AnyRef])

      assertSameStrict(LTT[Object | Option[String] | Nothing], LTT[Object])
      assertDebugSame(LTT[Object | Option[String] | Nothing], LTT[Object])

      assertSameStrict(LTT[Option[String] | Nothing], LTT[Option[String]])
      assertDebugSame(LTT[Option[String] | Nothing], LTT[Option[String]])
    }

    "tautological unions with Any/AnyRef/Matchable/Object are discarded from internal structure (Scala 3 specific, Matchable) (Tag)" in {
      assertSameStrict(Tag[Any | Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Any])
      assertDebugSame(Tag[Any | Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Any])

      assertSameStrict(Tag[Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Matchable])
      assertDebugSame(Tag[Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Matchable])

      assertSameStrict(Tag[AnyRef | Object | Option[String] | Nothing].tag, LTT[AnyRef])
      assertDebugSame(Tag[AnyRef | Object | Option[String] | Nothing].tag, LTT[AnyRef])

      assertSameStrict(Tag[Object | Option[String] | Nothing].tag, LTT[Object])
      assertDebugSame(Tag[Object | Option[String] | Nothing].tag, LTT[Object])

      assertSameStrict(Tag[Option[String] | Nothing].tag, LTT[Option[String]])
      assertDebugSame(Tag[Option[String] | Nothing].tag, LTT[Option[String]])
    }

    "normalize nested lambdas with relative depth indices" in {
      val innerLambda = LightTypeTagRef.Lambda(
        input = List(LightTypeTagRef.SymName.LambdaParamName(0, 0, 1)),
        output = LightTypeTagRef.NameReference(LightTypeTagRef.SymName.LambdaParamName(0, 0, 1))
      )

      val outerLambda = LightTypeTagRef.Lambda(
        input = List(LightTypeTagRef.SymName.LambdaParamName(0, 0, 1)),
        output = innerLambda
      )

      val normalizedOuter = LightTypeTagRef.normalizeLambda(outerLambda)

      assert(normalizedOuter.input.head.depth == -1)
      assert(
        normalizedOuter
          .output
          .asInstanceOf[LightTypeTagRef.Lambda].input.head.depth == -2
      )
    }

    "normalize IT1[List] lambdas to correct de Bruijn depths" in {
      val tag = LTT[IT1[List]]
      val debug = tag.toString

      // This will show something like:
      // λ %1:0 → λ %0:0 → IT1[%1:0[%0:0]]
      println(s"Normalized tag for IT1[List]:\n$debug")

      assert(debug.contains("%0:0"), "Expected inner lambda parameter to have depth 0")
      assert(debug.contains("%1:0"), "Expected outer lambda parameter to have depth -1")
    }

    "support top-level abstract types (Scala 3 specific, top level type aliases)" in {
      assertChildStrict(LTT[LightTypeTagTestT], LTT[String])
    }

    "support opaque types" in {
      object x {
        type T >: List[Int] <: List[Int]
        opaque type Opaque = List[Int]
        opaque type OpaqueSub <: List[Int] = List[Int]
      }

      assertNotChildStrict(LTT[x.Opaque], LTT[List[Int]])
      assertNotChildStrict(LTT[x.Opaque], LTT[Seq[Int]])
      assertNotChildStrict(LTT[x.Opaque], LTT[x.T])
      assertNotChildStrict(LTT[x.Opaque], LTT[x.OpaqueSub])

      assertChildStrict(LTT[x.OpaqueSub], LTT[List[Int]])
      assertChildStrict(LTT[x.OpaqueSub], LTT[Seq[Int]])
      assertChildStrict(LTT[x.T], LTT[Seq[Int]])
      assertChildStrict(LTT[x.OpaqueSub], LTT[x.T])
      assertDifferent(LTT[x.OpaqueSub], LTT[x.T])
    }

  }
}

type LightTypeTagTestT <: String
