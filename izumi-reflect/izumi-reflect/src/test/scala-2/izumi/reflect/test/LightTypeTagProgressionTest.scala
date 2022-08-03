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

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  "[progression] lightweight type tags (Scala 2)" should {

    "progression test: there should be no unexpected lambdas in bases db produced from nested existential types" in {
      trait L[ARRG0]

      trait Test0[+ARRG1]
      trait Test1[+ARRG2] extends Test0[ARRG2]

      type T1[AAA] = Test1[L[AAA]]

      val list_ = LTT[T1[_]]
      doesntWorkYetOnScala2 {
        assert(!list_.debug().contains("* λ %0 → izumi.reflect.test.LightTypeTagProgressionTest.Test0[+izumi.reflect.test.LightTypeTagProgressionTest.L[=?]]"))
      }
    }

  }

}
