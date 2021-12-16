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
import izumi.reflect.test.ID._
import izumi.reflect.test.TestModel.VarArgsAnyVal

// https://github.com/scala/bug/issues/11139
final case class testTag3[F[_]: TagK]() {
  type X = OptionT[F, Int]
  val res = Tag[X].tag
}

class TagTest extends SharedTagTest {

  import izumi.reflect.test.PlatformSpecific.fromRuntime

  override final val tagZ = Tag[String]

  trait H0
  trait H1 extends H0
  trait T1[A, B, C, D, E, F[_]]
  trait T2[A, B, C[_[_], _], D[_], E]
  trait Test[A, dafg, adfg, LS, L[_], SD, GG[A] <: L[A], ZZZ[_, _], S, SDD, TG]
  trait YX[V] extends XY[V]
  case class ZOBA[A, B, C](value: Either[B, C])
  type Const[A, B] = A
  trait ZIO[-R, +E, +A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type EitherR[-_, +L, +R] = Either[L, R]
  type EitherRSwap[-_, +L, +R] = Either[R, L]

  type F2To3[F[_, _], R, E, A] = F[E, A]

  trait BlockingIO3[F[_, _, _]]
  type BlockingIO[F[_, _]] = BlockingIO3[Lambda[(R, E, A) => F[E, A]]]

  trait BlockingIO3T[F[_, _[_], _]]
  type BlockingIOT[F[_[_], _]] = BlockingIO3T[Lambda[(R, `E[_]`, A) => F[E, A]]]

  class ApplePaymentProvider[F0[_]] extends H1

  trait DockerContainer[T]
  trait ContainerDef {
    type T

    def make(implicit t: Tag[T]) = {
      val _ = t
      Tag[DockerContainer[T]]
    }
  }

  trait Trait1 {
    def dep: Dep
  }
  trait Trait3[T <: Dep] extends Trait1 {
    def dep: T
  }

  "Tag (Scala 2)" should {

    "Work with term type prefixes" in {
      val zy = new ZY {}
      val zx = new ZY {}

      assertSame(Tag[zy.T].tag, fromLTag[zy.T])
      assertChild(Tag[zy.T].tag, fromLTag[zy.T])
      assertDifferent(Tag[zy.T].tag, fromLTag[zx.T])
      assertSame(Tag[zy.x.type].tag, fromLTag[zy.x.type])
      assertChild(Tag[zy.x.type].tag, fromLTag[zy.x.type])
      assertChild(Tag[zy.x.type].tag, fromLTag[String])
      assertChild(Tag[zy.x.type].tag, fromLTag[java.io.Serializable])
      assertDifferent(Tag[zy.x.type].tag, fromLTag[zx.x.type])
      assertSame(Tag[zy.y.type].tag, fromLTag[zy.y.type])
      assertChild(Tag[zy.y.type].tag, fromLTag[zy.y.type])
      assertChild(Tag[zy.y.type].tag, fromLTag[java.lang.Object])
      assertDifferent(Tag[zy.y.type].tag, fromLTag[zx.y.type])
      assertDifferent(Tag[zy.y.type].tag, fromLTag[zx.x.type])
    }

    "Support identity lambda type equality" in {
      val idTpeLTT = TagK[Identity].tag
      val idLambdaLTT = TagK[Lambda[A => A]].tag
      assert(idTpeLTT == idLambdaLTT)
    }

    "Work for structural concrete types" in {
      assert(Tag[{ def a: Int; def g: Boolean }].tag == fromRuntime[{ def a: Int; def g: Boolean }])
      assert(Tag[Int { def a: Int }].tag == fromRuntime[Int { def a: Int }])

      assert(Tag[With[str.type] with ({ type T = str.type with Int })].tag == fromRuntime[With[str.type] with ({ type T = str.type with Int })])
      assert(Tag[With[str.type] with ({ type T = str.type with Int })].tag != fromRuntime[With[str.type] with ({ type T = str.type with Long })])
    }

    "Work for any abstract type with available Tag while preserving additional refinement" in {
      def testTag[T: Tag] = Tag[T { def x: Int }]

      assert(testTag[String].tag == fromRuntime[String { def x: Int }])
    }

    "handle function local type aliases" in {
      def testTag3[F[_]: TagK] = {
        type X = OptionT[F, Int]

        Tag[X]
      }

      assert(testTag3[List].tag == fromRuntime[OptionT[List, Int]])
    }

    "Can dealias transparent type members with class type parameters inside them when a tag is summoned _inside_ the class, because LightTypeTags are not affected by https://github.com/scala/bug/issues/11139" in {
      assert(testTag3[List]().res == fromRuntime[OptionT[List, Int]])
    }

    "Work for an abstract type with available TagK when obscured by empty refinement" in {
      def testTagK[F[_]: TagK, T: Tag] = Tag[F[T {}] {}]

      assert(testTagK[Set, Int].tag == fromRuntime[Set[Int]])
    }

    "Work for an abstract type with available TagK when TagK is requested through an explicit implicit" in {
      def testTagK[F[_], T: Tag](implicit ev: HKTag[{ type Arg[C] = F[C] }]) = {
        val _ = ev
        Tag[F[T {}] {}]
      }

      assert(testTagK[Set, Int].tag == fromRuntime[Set[Int]])
    }

    "Work for an abstract type with available TagKK" in {
      def t1[F[_, _]: TagKK, T: Tag, G: Tag] = Tag[F[T, G]]

      assert(t1[ZOBA[Int, *, *], Int, String].tag == fromRuntime[ZOBA[Int, Int, String]])
    }

    "Handle Tags outside of a predefined set" in {
      type TagX[T[_, _, _[_[_], _], _[_], _]] = HKTag[{ type Arg[A, B, C[_[_], _], D[_], E] = T[A, B, C, D, E] }]

      def testTagX[F[_, _, _[_[_], _], _[_], _]: TagX, A: Tag, B: Tag, C[_[_], _]: TagTK, D[_]: TagK, E: Tag] = Tag[F[A, B, C, D, E]]

      val value = testTagX[T2, Int, String, OptionT, List, Boolean]
      assert(value.tag == fromRuntime[T2[Int, String, OptionT, List, Boolean]])
    }

    "Work for any configuration of parameters" in {

      def t1[A: Tag, B: Tag, C: Tag, D: Tag, E: Tag, F[_]: TagK]: Tag[T1[A, B, C, D, E, F]] = Tag[T1[A, B, C, D, E, F]]

      type ZOB[A, B, C] = Either[B, C]

      assert(
        t1[Int, Boolean, ZOB[Unit, Int, Int], TagK[Option], Nothing, ZOB[Unit, Int, *]].tag
        == fromRuntime[T1[Int, Boolean, Either[Int, Int], TagK[Option], Nothing, Either[Int, *]]]
      )

      def t2[A: Tag, dafg: Tag, adfg: Tag, LS: Tag, L[_]: TagK, SD: Tag, GG[A] <: L[A]: TagK, ZZZ[_, _]: TagKK, S: Tag, SDD: Tag, TG: Tag]
        : Tag[Test[A, dafg, adfg, LS, L, SD, GG, ZZZ, S, SDD, TG]] =
        Tag[Test[A, dafg, adfg, LS, L, SD, GG, ZZZ, S, SDD, TG]]

      assert(
        t2[TagTest.this.Z, TagTest.this.Z, T1[
          ZOB[String, Int, Byte],
          String,
          String,
          String,
          String,
          List
        ], TagTest.this.Z, XY, TagTest.this.Z, YX, Either, TagTest.this.Z, TagTest.this.Z, TagTest.this.Z].tag
        == fromRuntime[Test[String, String, T1[Either[Int, Byte], String, String, String, String, List], String, XY, String, YX, Either, String, String, String]]
      )
    }

    "handle Swap type lambda" in {
      def t1[F[_, _]: TagKK, A: Tag, B: Tag] = Tag[F[A, B]]

      assert(t1[Swap, Int, String].tag == fromRuntime[Either[String, Int]])
    }

    "Assemble from higher than TagKK tags" in {
      def tag[T[_[_], _]: TagTK, F[_]: TagK, A: Tag] = Tag[T[F, A]]

      assert(tag[OptionT, Option, Int].tag == fromRuntime[OptionT[Option, Int]])
    }

    "Handle abstract types instead of parameters" in {
      trait T1 {
        type F[F0[_], A0] = OptionT[F0, A0]
        type C[_, _]
        type G[_]
        type A
        type B

        def x: Tag[F[G, Either[A, B]]]
      }

      val t1: T1 {
        type G[T] = List[T]
        type C[A0, B0] = Either[A0, B0]
        type A = Int
        type B = Byte
      } = new T1 {
        type G[T] = List[T]
        type C[A0, B0] = Either[A0, B0]
        type A = Int
        type B = Byte

        // Inconsistent handling of type aliases by scalac...
        // No TagK for G, but if G is inside an object or enclosing class
        // then there is a TagK
        val g: TagK[G] = TagK[List]

        final val x: Tag[F[G, Either[A, B]]] = {
          implicit val g0: TagK[G] = g
          val _ = g0
          Tag[F[G, C[A, B]]]
        }
      }

      assert(t1.x.tag == fromRuntime[OptionT[List, Either[Int, Byte]]])
    }

    "Can create custom type tags to support bounded generics, e.g. <: Dep in TagK" in {
      type `TagK<:Dep`[K[_ <: Dep]] = HKTag[{ type Arg[A <: Dep] = K[A] }]

      implicitly[`TagK<:Dep`[Trait3]].tag.withoutArgs =:= LTag[Trait3[Nothing]].tag.withoutArgs
    }

    "can find HKTag when obscured by type lambda" in {
      assertCompiles("HKTag.hktagFromTagMacro[{ type Arg[C] = Option[C] }]")
      assertCompiles("HKTag.hktagFromTagMacro[({ type l[F[_]] = { type Arg[C] = F[C] } })#l[Option]]")
    }

    "return expected class tag" in {
      assert(Tag[List[_] with Set[_]].closestClass eq classOf[scala.collection.immutable.Iterable[_]])
      assert(!Tag[List[_] with Set[_]].hasPreciseClass)

      assert(Tag[AnyVal].closestClass eq classOf[AnyVal])
      assert(!Tag[AnyVal].hasPreciseClass)

      assert(Tag[String with Int].closestClass eq classOf[AnyVal])
      assert(!Tag[String with Int].hasPreciseClass)

      assert(Tag[List[Int]].closestClass eq classOf[List[_]])
      assert(Tag[List[Int]].hasPreciseClass)
      assert(Tag[H1].hasPreciseClass)

      assert(Tag[ZY#T].closestClass eq classOf[Any])
      assert(!Tag[ZY#T].hasPreciseClass)
    }

    "resolve TagK from TagKK" in {
      def getTag[F[+_, +_]: TagKK] = TagK[F[Throwable, *]]
      val tagEitherThrowable = getTag[Either].tag
      val tag = TagK[Either[Throwable, *]].tag

      assert(tagEitherThrowable =:= tag)
      assert(tagEitherThrowable <:< tag)
      assert(tagEitherThrowable <:< TagK[Either[Any, *]].tag)
      assert(TagK[Either[Nothing, *]].tag <:< tagEitherThrowable)
    }

    "can materialize TagK for type lambdas that close on a generic parameter with available Tag" in {
      def partialEitherTagK[A: Tag] = TagK[Either[A, *]]

      val tag = partialEitherTagK[Int].tag
      val expectedTag = TagK[Either[Int, *]].tag

      assert(tag =:= expectedTag)
    }

    "can materialize TagK for type lambdas that close on a generic parameter with available Tag when the constructor is a type parameter" in {
      def partialFTagK[F[_, _]: TagKK, A: Tag] = TagK[F[A, *]]

      val tag = partialFTagK[Either, Int].tag
      val expectedTag = TagK[Either[Int, *]].tag

      assert(tag =:= expectedTag)
    }

    "type parameter covariance works after combine" in {
      def getTag[F[+_, +_]: TagKK] = TagK[F[Throwable, *]]
      val tagEitherThrowable = getTag[Either].tag
      val tagEitherSerializable = TagK[Either[java.io.Serializable, *]]
      assert(tagEitherThrowable <:< tagEitherSerializable.tag)
    }

    "combine Const Lambda to TagK" in {
      def get[F[_, _]: TagKK] = TagK[F[Int, *]]
      val tag = get[Const]

      assert(tag.tag =:= TagK[Const[Int, *]].tag)
      assert(tag.tag <:< TagK[Const[AnyVal, *]].tag)
      assert(tag.tag.hashCode() == TagK[Const[Int, *]].tag.hashCode())
    }

    "combined TagK 3 & 2 parameter coherence" in {
      def get[F[+_, +_]: TagKK] = TagK[F[Throwable, *]]
      val tag = get[IO]

      assert(tag.tag =:= TagK[IO[Throwable, *]].tag)
      assert(tag.tag <:< TagK[IO[Throwable, *]].tag)
      assert(tag.tag <:< TagK[IO[Any, *]].tag)
    }

    "resolve TagKK from an odd higher-kinded Tag with swapped & ignored parameters (low-level)" in {
      type Lt[F[_, _, _], _1, _2, _3] = F[_2, _3, _1]

      val ctorTag: LightTypeTag = implicitly[Tag.auto.T[Lt]].tag
      val eitherRSwapTag = LTagK3[EitherRSwap].tag
      val throwableTag = LTag[Throwable].tag

      val combinedTag = HKTag
        .appliedTagNonPosAux(
          classOf[Any],
          ctor = ctorTag,
          args = List(
            Some(eitherRSwapTag),
            Some(throwableTag),
            None,
            None
          )
        ).tag
      val expectedTag = TagKK[Lt[EitherRSwap, Throwable, *, *]].tag
      assert(combinedTag =:= expectedTag)
    }

    "resolve TagKK from an odd higher-kinded Tag with swapped & ignored parameters" in {
      def getTag[F[-_, +_, +_]: TagK3] = TagKK[F[*, *, Throwable]]
      val tagEitherSwap = getTag[EitherRSwap].tag
      val tagEitherThrowable = getTag[EitherR].tag

      val expectedTagSwap = TagKK[EitherRSwap[*, *, Throwable]].tag
      val expectedTagEitherThrowable = TagKK[EitherR[*, *, Throwable]].tag

      assert(!(tagEitherSwap =:= expectedTagEitherThrowable))
      assert(tagEitherSwap =:= expectedTagSwap)
      assert(tagEitherThrowable =:= expectedTagEitherThrowable)
      assert(tagEitherSwap <:< expectedTagSwap)
      assert(tagEitherSwap <:< TagKK[EitherRSwap[*, *, Any]].tag)
      assert(TagKK[EitherRSwap[*, *, Nothing]].tag <:< tagEitherSwap)
    }

    "combine higher-kinded types without losing ignored type arguments" in {
      def mk[F[+_, +_]: TagKK] = Tag[BlockingIO[F]]
      val tag = mk[IO]

      assert(tag.tag == Tag[BlockingIO[IO]].tag)
    }

    "resolve a higher-kinded type inside a named type lambda with ignored type arguments" in {
      def mk[F[+_, +_]: TagKK] = Tag[BlockingIO3[F2To3[F, *, *, *]]]
      val tag = mk[IO]

      assert(tag.tag == Tag[BlockingIO[IO]].tag)
    }

    "resolve a higher-kinded type inside an anonymous type lambda with ignored & higher-kinded type arguments" in {
      def mk[F[_[_], _]: TagTK] = Tag[BlockingIO3T[Lambda[(`-R`, `E[_]`, `A`) => F[E, A]]]]
      val tag = mk[OptionT]

      assert(tag.tag == Tag[BlockingIOT[OptionT]].tag)
    }

    "correctly resolve a higher-kinded nested type inside a named swap type lambda" in {
      def mk[F[+_, +_]: TagKK] = Tag[BIOService[SwapF2[F, *, *]]]
      val tag = mk[Either]

      assert(tag.tag == Tag[BIOService[SwapF2[Either, *, *]]].tag)
      assert(tag.tag == Tag[BIOService[Swap]].tag)
      assert(tag.tag == Tag[BIOService[Lambda[(E, A) => Either[A, E]]]].tag)
    }

    "correctly resolve a higher-kinded nested type inside an anonymous swap type lambda" in {
      def mk[F[+_, +_]: TagKK] = Tag[BIOService[Lambda[(E, A) => F[A, E]]]]
      val tag = mk[Either]

      assert(tag.tag == Tag[BIOService[SwapF2[Either, *, *]]].tag)
      assert(tag.tag == Tag[BIOService[Swap]].tag)
      assert(tag.tag == Tag[BIOService[Lambda[(E, A) => Either[A, E]]]].tag)
    }

    "correctly resolve abstract types inside traits when summoned inside trait" in {
      val a = new ContainerDef {}
      val b = new ContainerDef {}

      assert(a.make.tag == Tag[DockerContainer[a.T]].tag)
      assert(a.make.tag != Tag[DockerContainer[b.T]].tag)
      assert(Tag[DockerContainer[a.T]].tag == Tag[DockerContainer[a.T]].tag)

      val zy = new ZY {}
      assert(zy.tagT.getMessage contains "could not find implicit value for Tag[")
      assert(zy.tagU.getMessage contains "could not find implicit value for Tag[")
      assert(zy.tagV.getMessage contains "could not find implicit value for Tag[")
      assert(zy.tagA.isSuccess)
    }

    "can resolve parameters in structural types" in {
      def t[X: Tag]: Tag[{ type T = X }] = Tag[{ type T = X }]

      assertSame(t[Int].tag, Tag[{ type T = Int }].tag)
    }

    "can resolve Tags of TagK's themselves correctly" in {
      trait X[A, B, C]

      def tagk[F[_]: TagK]: Tag[TagK[F]] = Tag[TagK[F]]
      def tagkk[F[_, _]: TagKK]: Tag[TagKK[F]] = Tag[TagKK[F]]
      def tagk3[F[_, _, _]: TagK3]: Tag[TagK3[F]] = Tag[TagK3[F]]
      def tagtk[F[_[_], _]: TagTK]: Tag[TagTK[F]] = Tag[TagTK[F]]

      assertChild(tagk[List].tag, Tag[TagK[List]].tag)
      assertSame(tagkk[Either].tag, Tag[TagKK[Either]].tag)
      assertSame(tagk3[X].tag, Tag[TagK3[X]].tag)
      assertSame(tagtk[OptionT].tag, Tag[TagTK[OptionT]].tag)
    }

    "regression test: ignore function-local anonymous classes (https://github.com/zio/zio/issues/4285)" in {
      class ZIO[-R, +E, +A](val a: Any) {
        def map[B](f: A => B): ZIO[R, E, B] = new ZIO(f)
        def toLayer[A1 >: A: Tag]: ZLayer[R, E, Has[A1]] = new ZLayer(Tag[Has[A1]])
      }
      class ZLayer[-R, +E, +A](val t: Tag[_ <: A])
      final class Has[X]

      type UIO[T] = ZIO[Any, Nothing, T]
      def f[T]: UIO[T] = new ZIO(1)
      trait S[T] { val param: T }

      def reproduce[T: Tag]: ZLayer[Any, Nothing, Has[S[T]]] = {
        f[T].map(p => new S[T] { override val param: T = p }).toLayer
      }

      assert(reproduce[Unit].t.tag == Tag[Has[S[Unit]]].tag)
    }

    "regression test: resolve correct closestClass for Scala vararg AnyVal (https://github.com/zio/izumi-reflect/issues/224)" in {
      assert(Tag[VarArgsAnyVal].closestClass == classOf[scala.Seq[Any]])
    }

    "equal path-dependant tags for singleton types are expected to be equal" in {
      // see https://github.com/zio/izumi-reflect/issues/192
      object Foo {
        val bar = "bar"
        object Bar

        val t1 = Tag[bar.type]
        val t2 = Tag[Foo.bar.type]
        val t3 = Tag[Foo.this.bar.type]

        val T1 = Tag[Bar.type]
        val T2 = Tag[Foo.Bar.type]
        val T3 = Tag[Foo.this.Bar.type]
      }

      import Foo._
      assert(t1.tag =:= t3.tag)
      assert(t2.tag =:= t3.tag)

      assert(T1.tag =:= T3.tag)
      assert(T2.tag =:= T3.tag)
    }

  }

}
