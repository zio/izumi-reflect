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

import izumi.reflect._
import izumi.reflect.macrortti._
import izumi.reflect.test.TestModel.VarArgsAnyVal

class TagTest extends SharedTagTest {

  import izumi.reflect.test.PlatformSpecific.fromRuntime

  override final val tagZ = Tag[String]

  trait H0
  trait H1 extends H0
  trait T1[A, B, C, D, E, F[_]]
  trait T2[A, B, C[_[_], _], D[_], E]
  trait Test[A, dafg, adfg, LS, L[_], SD, GG[A] <: L[A], ZZZ[_, _], S, SDD, TG]
  type Const[A, B] = A
  trait ZIO[-R, +E, +A]
  type IO[+E, +A] = ZIO[Any, E, A]

  class ApplePaymentProvider[F0[_]] extends H1

  trait Trait1 {
    def dep: Dep
  }
  trait Trait3[T <: Dep] extends Trait1 {
    def dep: T
  }

  "Tag (Scala 2)" should {

    "Work for an abstract type with available TagK when TagK is requested through an explicit implicit (Scala 2 HKTag Syntax)" in {
      def testTagK[F[_], T: Tag](implicit ev: HKTag[{ type Arg[C] = F[C] }]) = {
        val _ = ev
        Tag[F[T {}] {}]
      }

      assert(testTagK[Set, Int].tag == fromRuntime[Set[Int]])
    }

    "Handle Tags outside of a predefined set (Scala 2 HKTag Syntax)" in {
      type TagX[T[_, _, _[_[_], _], _[_], _]] = HKTag[{ type Arg[A, B, C[_[_], _], D[_], E] = T[A, B, C, D, E] }]

      def testTagX[F[_, _, _[_[_], _], _[_], _]: TagX, A: Tag, B: Tag, C[_[_], _]: TagTK, D[_]: TagK, E: Tag] = Tag[F[A, B, C, D, E]]

      val value = testTagX[T2, Int, String, OptionT, List, Boolean]
      assert(value.tag == fromRuntime[T2[Int, String, OptionT, List, Boolean]])
    }

    "Can create custom type tags to support bounded generics, e.g. <: Dep in TagK (Scala 2 HKTag Syntax)" in {
      type `TagK<:Dep`[K[_ <: Dep]] = HKTag[{ type Arg[A <: Dep] = K[A] }]

      implicitly[`TagK<:Dep`[Trait3]].tag.withoutArgs =:= LTag[Trait3[Nothing]].tag.withoutArgs
    }

    "can find HKTag when obscured by type lambda (Scala 2 HKTag Syntax)" in {
      assertCompiles("HKTag.hktagFromTagMacro[{ type Arg[C] = Option[C] }]")
      assertCompiles("HKTag.hktagFromTagMacro[({ type l[F[_]] = { type Arg[C] = F[C] } })#l[Option]]")
    }

  }

}
