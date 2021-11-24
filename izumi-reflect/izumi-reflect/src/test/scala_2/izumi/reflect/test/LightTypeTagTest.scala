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

  "lightweight type tags" should {

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
      Z

      assertSame(LTT[a1.A], LTT[Z.X#A])
    }

    "strong summons test (Scala 2 specific)" in {
      assertCompiles("def x1 = { object x { type T <: { type Array } }; LTag[x.T#Array]; () }")
    }

    "`typeArgs` works (Scala 2 syntax)" in {
      assert(`LTT[_[_]]`[T0[List, *[_]]].typeArgs == List(`LTT[_]`[List]))
    }

    "combine higher-kinded type lambdas without losing ignored type arguments (Scala 2 syntax)" in {
      val tag = `LTT[_[+_,+_]]`[Lambda[`F[+_, +_]` => BlockingIO3[Lambda[(`-R`, `+E`, `+A`) => F[E, A]]]]]
      val res = tag.combine(`LTT[_,_]`[IO])
      assertSame(res, LTT[BlockingIO[IO]])
    }

  }
}
