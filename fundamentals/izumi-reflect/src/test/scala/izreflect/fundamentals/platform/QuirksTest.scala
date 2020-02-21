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

package izreflect.fundamentals.platform

import izreflect.fundamentals.platform.language.Quirks._
import izreflect.fundamentals.platform.language.unused
import org.scalatest.wordspec.AnyWordSpec

class QuirksTest extends AnyWordSpec {
  def boom: Int = throw new RuntimeException()

  "Discarder" should {
    "forget values effectlessly" in {
      boom.forget
      forget(boom)
      forget(boom, boom)
    }

    "forget values effectlessly when only discard method imported" in {
      forget(boom)
      forget(boom, boom)
    }

    "discard values effectfully" in {
      intercept[RuntimeException](boom.discard())
      intercept[RuntimeException](discard(boom))
      intercept[RuntimeException](discard(boom, boom))
    }

    "ignore values with unused annotation" in {
      def x(@unused x: => Int): Unit = ()
      x(boom)
    }
  }
}


