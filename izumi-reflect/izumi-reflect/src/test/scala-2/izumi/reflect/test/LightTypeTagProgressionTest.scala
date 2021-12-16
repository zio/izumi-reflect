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

import izumi.reflect.macrortti.LTT
import izumi.reflect.{Tag, TagK3, TagKK}
import izumi.reflect.test.TestModel.{BIO2, IO, ZIO}

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  "[progression] lightweight type tags (Scala 2)" should {

    "progression test: reported in https://github.com/zio/izumi-reflect/issues/82, fails to soundly convert trifunctor hkt to bifunctor when combining tags (Dotty fails to compile)" in {
      def tag[F[-_, +_, +_]: TagK3] = Tag[BIO2[F[Any, +*, +*]]]

      doesntWorkYetOnScala2 {
        assertChild(tag[ZIO].tag, Tag[BIO2[IO]].tag)
      }
      doesntWorkYetOnScala2 {
        assertChild(Tag[BIO2[IO]].tag, tag[ZIO].tag)
      }
      doesntWorkYetOnScala2 {
        assertSame(tag[ZIO].tag, Tag[BIO2[IO]].tag)
      }
    }

    "progression test: reported in https://github.com/zio/izumi-reflect/issues/83, fails to convert trifunctor tag to bifunctor tag (Dotty fails to compile)" in {
      def direct[F[+_, +_]: TagKK] = Tag[BIO2[F]]
      def indirectFrom3[F[-_, +_, +_]: TagK3] = direct[F[Any, +*, +*]]

      doesntWorkYetOnScala2 {
        assertSame(direct[ZIO[Any, +*, +*]].tag, indirectFrom3[ZIO].tag)
      }
    }

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
