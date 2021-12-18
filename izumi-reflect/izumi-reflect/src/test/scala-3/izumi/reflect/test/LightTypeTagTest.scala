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
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, Boundaries, Lambda}

import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

class LightTypeTagTest extends SharedLightTypeTagTest {

  import TestModel._

  "lightweight type tags (Dotty)" should {

    "support string constant types (Scala 2.13+ syntax)" in {
      assertDifferent(LTT["abc"], LTT[String])
      assertDifferent(LTT["abc"], LTT["cba"])
      assertChild(LTT["abc"], LTT["abc"])
      assertChild(LTT["abc"], LTT[String])
      assertDeepNotChild(LTT[String], LTT["abc"])
    }

    "support typetag combination (Dotty syntax)" in {
      assertCombine(`LTT[_[_]]`[[K[_]] =>> T0[Id, K]], `LTT[_]`[FP], LTT[T0[Id, FP]])
    }

    "`typeArgs` works (Dotty syntax)" in {
      assert(`LTT[_[_]]`[[T[_]] =>> T0[List, T]].typeArgs == List(`LTT[_]`[List]))
    }

    "tautological intersections with Matchable are discarded from internal structure" in {
      assertDeepSame(LTT[Matchable with Option[String]], LTT[Option[String]])
      assertDebugSame(LTT[Matchable with Option[String]], LTT[Option[String]])
    }

  }
}
