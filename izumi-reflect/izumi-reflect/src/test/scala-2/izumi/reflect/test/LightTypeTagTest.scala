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

    "support structural & refinement type equality (Scala 2 specific, generic type projection)" in {
      val a1 = new C {
        override type A = Int
      }
      object Z {
        type X <: { type A = Int }
      }
      val _ = (a1, Z)

      assertSame(LTT[a1.A], LTT[Z.X#A])
    }

    "strong summons test (Scala 2 specific, generic type projection)" in {
      assertCompiles("def x1 = { object x { type T <: { type Array } }; LTag[x.T#Array]; () }")
    }

  }
}
