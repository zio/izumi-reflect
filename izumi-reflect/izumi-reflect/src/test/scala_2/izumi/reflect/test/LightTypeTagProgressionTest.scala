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
import org.scalatest.exceptions.TestFailedException

class LightTypeTagProgressionTest extends TagAssertions {

  import TestModel._

  "lightweight type tags" should {
    "progression test: can't support subtyping of type prefixes" in {
      val a = new C {}

      intercept[TestFailedException] {
        assertChild(LTT[a.A], LTT[C#A])
      }
    }

    "progression test: can't support subtyping of concrete type projections" in {
      trait A {

        trait T

      }
      trait B extends A

      val tagA = LTT[A#T]
      val tagB = LTT[B#T]
      assertSame(LTT[A#T], LTT[A#T])
      assertDifferent(LTT[B#T], LTT[A#T])
      intercept[TestFailedException] {
        assertChild(tagB, tagA)
      }
    }

    "progression test: wildcards are not supported" in {
      intercept[TestFailedException] {
        assertChild(LTT[Set[Int]], LTT[Set[_]])
      }
    }

    "progression test: subtype check fails when child type has absorbed a covariant type parameter of the supertype" in {
      assertChild(LTT[Set[Int]], LTT[Iterable[AnyVal]])

      val tagF3 = LTT[F3]
      assertChild(tagF3, LTT[F2[Int]])

      intercept[TestFailedException] {
        assertChild(tagF3, LTT[F2[AnyVal]])
      }
    }

  }
}
