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

import izumi.reflect.macrortti._

class LightTypeTagTest extends SharedLightTypeTagTest {

  import TestModel._

  "lightweight type tags (Scala 2)" should {

    "wildcards are not supported (wildcard=Any, historically due to behavior of .dealias on 2.12/13, see scala.reflect.internal.tpe.TypeMaps#ExistentialExtrapolation)" in {
      assertDifferent(LTT[Set[_]], LTT[Set[Any]])
      assertDifferent(LTT[List[_]], LTT[List[Any]])

      assertChild(LTT[Set[Int]], LTT[Set[_]])
      assertNotChild(LTT[Set[_]], LTT[Set[Int]])

      assertChild(LTT[List[Int]], LTT[List[_]])
      assertNotChild(LTT[List[_]], LTT[List[Int]])

      assertChild(LTT[Int => Int], LTT[_ => Int])
    }

    "wildcards with bounds are not supported (upper bound is the type, historically due to behavior of .dealias on 2.12/13, see scala.reflect.internal.tpe.TypeMaps#ExistentialExtrapolation)" in {
      assertDifferent(LTT[Option[W1]], LTT[Option[_ <: W1]])
      assertDifferent(LTT[Option[H2]], LTT[Option[_ >: H4 <: H2]])
      assertDifferent(LTT[Option[Any]], LTT[Option[_ >: H4]])
    }

    "support typetag combination (Scala 2 syntax)" in {
      assertCombine(`LTT[_[_]]`[T0[Id, *[_]]], `LTT[_]`[FP], LTT[T0[Id, FP]])
    }

    "support complex type lambdas (Scala 2 syntax)" in {
      assertSame(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[Lambda[(A, B) => FM2[(B, A)]]])
      assertSame(`LTT[_[_]]`[NestedTL2[W1, W2, *[_]]], `LTT[_[_]]`[Lambda[G[_] => FM2[G[S[W2, W1]]]]])
      assertChild(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[Lambda[(A, B) => FM2[(B, A)]]])
    }

    "support structural & refinement type equality (Scala 2 specific)" in {
      val a1 = new C {
        override type A = Int
      }
      object Z {
        type X <: { type A = Int }
      }
      val _ = (a1, Z)

      assertSame(LTT[a1.A], LTT[Z.X#A])
    }

    "strong summons test (Scala 2 specific)" in {
      assertCompiles("def x1 = { object x { type T <: { type Array } }; LTag[x.T#Array]; () }")
    }

    "`typeArgs` works (Scala 2 syntax)" in {
      val tag = `LTT[_[_]]`[T0[List, *[_]]]
      val tagApplied = tag.combine(`LTT[_]`[Option])
      assertSame(tagApplied, LTT[T0[List, Option]])
      assert(tag.typeArgs == List(`LTT[_]`[List]))
      assert(tagApplied.typeArgs == List(`LTT[_]`[List], `LTT[_]`[Option]))
    }

    "combine higher-kinded type lambdas without losing ignored type arguments (Scala 2 syntax)" in {
      val tag = `LTT[_[+_,+_]]`[Lambda[`F[+_, +_]` => BlockingIO3[Lambda[(`-R`, `+E`, `+A`) => F[E, A]]]]]
      val res = tag.combine(`LTT[_,_]`[IO])
      assertSame(res, LTT[BlockingIO[IO]])
    }

  }
}
