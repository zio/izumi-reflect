package izumi.reflect.test

import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag210
import izumi.reflect.macrortti._
import izumi.reflect.test.ID._
import izumi.reflect.test.TestModel._
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect._
import izumi.reflect.macrortti.LightTypeTagRef.{FullReference, LambdaParameter, NameReference, SymName, TypeParam}
import org.scalatest.Assertions
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.StaticAnnotation
import scala.collection.immutable.List
import scala.collection.mutable
import scala.util.Try

object ID {
  type id[A] = A
  type Identity[+A] = A
}

trait Clock
object ClockLive extends Clock

trait ZY extends Assertions {
  type T
  type U = T
  type V = List[T]
  type A = List[Option[Int]]
  val x: String = "5"
  object y
  trait Y

  val tagT = intercept[TestFailedException](assertCompiles("izumi.reflect.Tag[T]"))
  val tagU = intercept[TestFailedException](assertCompiles("izumi.reflect.Tag[U]"))
  val tagV = intercept[TestFailedException](assertCompiles("izumi.reflect.Tag[V]"))
  val tagA = Try(assertCompiles("izumi.reflect.Tag[A]"))
}

trait XY[Y] {
  type Z = id[Y]

  implicit def tagZ: Tag[Z]
}

case class OptionT[F[_], A](value: F[Option[A]])

final case class testTag[T: Tag]() {
  type X[A] = Either[Int, A]
  type Y = T
  val res = Tag[X[Y {}]]
}

final case class testTag2[T: Tag]() {
  type X = List[T]
  val res = Tag[X]
}

trait TXU[A, B, C[_[_], _], D[_], E]

abstract class SharedTagTest extends AnyWordSpec with XY[String] with TagAssertions with InheritedModel {

  type Abstract
  type Id[A] = A
  type Id1[F[_], A] = F[A]

  import izumi.reflect.test.PlatformSpecific.fromRuntime

  final val str = "str"

  final class With[T] extends StaticAnnotation

  case class ZOBA[A, B, C](value: Either[B, C])

  trait Test[A, dafg, adfg, LS, L[_], SD, GG[B] <: L[B], ZZZ[_, _], S, SDD, TG]
  trait T1[A, B, C, D, E, F[_]]
  trait YX[V] extends XY[V]

  trait DockerContainer[T]
  trait ContainerDef {
    type T

    def make(implicit t: Tag[T]) = {
      val _ = t
      Tag[DockerContainer[T]]
    }
  }

  "Tag (all versions)" should {

    "Work for any concrete type" in {
      assert(Tag[Int].tag == fromRuntime[Int])
      assert(Tag[Set[String]].tag == fromRuntime[Set[String]])
      assert(Tag[Map[Boolean, Double]].tag == fromRuntime[Map[Boolean, Double]])
      assert(Tag[_ => Unit].tag == fromRuntime[_ => Unit])
      assert(Tag[Unit => _].tag == fromRuntime[Unit => _])
      assert(Tag[_ => _].tag == fromRuntime[_ => _])

      assert(Tag[Any].tag == fromRuntime[Any])
      assert(Tag[Nothing].tag == fromRuntime[Nothing])
      assert(Tag[Any => Nothing].tag == fromRuntime[Any => Nothing])
      assert(Tag[Nothing => Any].tag == fromRuntime[Nothing => Any])

      assert(Tag[With[Any]].tag == fromRuntime[With[Any]])
      assert(Tag[With[Nothing]].tag == fromRuntime[With[Nothing]])
      assert(Tag[With[_]].tag == fromRuntime[With[_]])

      assert(Tag[Int with String].tag == fromRuntime[Int with String])

      assert(Tag[str.type].tag == fromRuntime[str.type])
    }

    "Support identity lambda type equality" in {
      val idTpeLTT = TagK[Identity].tag
      val idLambdaLTT = TagK[λ[A => A]].tag
      assert(idTpeLTT == idLambdaLTT)
    }

    "regression test for https://github.com/zio/izumi-reflect/issues/98" in {
      object SomeService {
        trait Service[T]

        final case class Foo()

        val tag1: Tag[Service[Foo]] = Tag[Service[Foo]]
      }

      object IzumiReflectTagEqualRegression {
        import SomeService._

        def test(): Unit = {
          val tag1: Tag[Service[Foo]] = SomeService.tag1
          val tag2: Tag[Service[Foo]] = Tag[Service[Foo]]

          val rtTag1 = PickleImpl.serializeIntoString(tag1.tag.ref, LightTypeTag.lttRefSerializer)
          val rtTag2 = PickleImpl.serializeIntoString(tag2.tag.ref, LightTypeTag.lttRefSerializer)

          assert(tag1.tag.ref == tag2.tag.ref)

          assert(rtTag1 == tag1.tag.asInstanceOf[ParsedLightTypeTag210].refString)
          assert(rtTag2 == tag2.tag.asInstanceOf[ParsedLightTypeTag210].refString)

          assert(rtTag1 == rtTag2)

          assert(tag1.tag == tag2.tag)
          ()
        }
      }

      IzumiReflectTagEqualRegression.test()
    }

    "Work for any concrete type (dotty failures)" in {
      assert(Tag[this.Z].tag == fromRuntime[this.Z])
      assert(Tag[TagTest#Z].tag == fromRuntime[TagTest#Z])
    }

    "Work for any abstract type with available Tag when obscured by empty refinement" in {
      def testTag[T: Tag] = Tag[T {}]

      assert(testTag[String].tag == fromRuntime[String])
    }

    "handle function local type aliases" in {

      def testTag[T: Tag] = {
        type X[A] = Either[Int, A]

        Tag[X[T {}]]
      }

      assert(testTag[String].tag == fromRuntime[Either[Int, String]])

      def testTag2[T: Tag] = {
        type X = List[T]

        Tag[X]
      }

      assert(testTag2[String].tag == fromRuntime[List[String]])

      def testTag3[F[_]: TagK] = {
        type X = OptionT[F, Int]

        Tag[X]
      }

      assert(testTag3[List].tag == fromRuntime[OptionT[List, Int]])
    }

    "Can dealias transparent type members with class type parameters inside them when a tag is summoned _inside_ the class, because LightTypeTags are not affected by https://github.com/scala/bug/issues/11139" in {
      assert(testTag[String]().res.tag == fromRuntime[Either[Int, String]])
      assert(testTag2[String]().res.tag == fromRuntime[List[String]])
      assert(testTag3[List]().res == fromRuntime[OptionT[List, Int]])
    }

    "Tag.auto.T kind inference macro works for known cases" in {
      def x[T[_]: Tag.auto.T]: TagK[T] = implicitly[Tag.auto.T[T]]

      def x2[T[_, _]: Tag.auto.T]: TagKK[T] = implicitly[Tag.auto.T[T]]

      def x3[T[_, _, _[_[_], _], _[_], _]](implicit x: Tag.auto.T[T]): Tag.auto.T[T] = x

      val b1 = x[Option].tag =:= TagK[Option].tag
      val b2 = x2[Either].tag =:= TagKK[Either].tag
      val b3 = implicitly[Tag.auto.T[OptionT]].tag =:= TagTK[OptionT].tag
      val b4 = x3[TXU].tag.withoutArgs =:= LTag[TXU[Nothing, Nothing, Nothing, Nothing, Nothing]].tag.withoutArgs

      assert(b1)
      assert(b2)
      assert(b3)
      assert(b4)
    }

    "Shouldn't work for any abstract type without available TypeTag or Tag or TagK" in {
      assertTypeError("""
      def testTag[T] = Tag[T]
      def testTagK[F[_], T] = Tag[F[T]]
         """)
    }

    "handle Id type lambda" in {
      assert(TagK[Id].tag == TagK[Id].tag)
      assert(TagK[Id].tag != TagTK[Id1].tag)
    }

    "handle Id1 type lambda" in {
      assert(TagTK[Id1].tag == TagTK[Id1].tag)
      assert(TagTK[Id1].tag != TagK[Id].tag)
    }

    "handle singleton types" in {
      assertChild(Tag[ClockLive.type].tag, Tag[Clock].tag)
    }

    "handle nested intersection aliases" in {
      type Inner = Int with String
      type Outer = Boolean with Inner
      assertChild(Tag[Outer].tag, Tag[Boolean with Int with String].tag)
      assertChild(Tag[Boolean with Int with String].tag, Tag[Outer].tag)
      assertSame(Tag[Outer].tag, Tag[Boolean with Int with String].tag)

      assertNotChild(Tag[Outer].tag, Tag[Boolean with Int with String with Unit].tag)
      assertNotChild(Tag[Boolean with Int with String].tag, Tag[Outer with Unit].tag)

      assertChild(Tag[Boolean with Int with String].tag, Tag[CharSequence].tag)
      assertChild(Tag[Outer].tag, Tag[CharSequence].tag)

      // there should be no refinements or intersections in bases
      assert(!Tag[Outer].tag.debug().contains("<refinement>"))
      assert(!Tag[Outer].tag.debug().contains("<none>"))
      assert(!Tag[Outer].tag.debug().contains("- {"))
    }

    "handle nested refined intersection aliases" in {
      type Inner = ((Int with (String {})) {}) @IdAnnotation("y")
      type Outer = Boolean with (((Inner {}) @IdAnnotation("x")) {})
      assertChild(Tag[Outer].tag, Tag[Boolean with Int with String].tag)
      assertChild(Tag[Boolean with Int with String].tag, Tag[Outer].tag)
      assertSame(Tag[Outer].tag, Tag[Boolean with Int with String].tag)

      assertNotChild(Tag[Outer].tag, Tag[Boolean with Int with String with Unit].tag)
      assertNotChild(Tag[Boolean with Int with String].tag, Tag[Outer with Unit].tag)

      assertChild(Tag[Outer].tag, Tag[CharSequence].tag)

      // there should be no refinements or intersections in bases
      assert(!Tag[Outer].tag.debug().contains("<refinement>"))
      assert(!Tag[Outer].tag.debug().contains("<none>"))
      assert(!Tag[Outer].tag.debug().contains("- {"))
    }

    "simple combined Tag" in {
      def get[F[_]: TagK] = Tag[ApplePaymentProvider[F]]
      val tag = get[Identity]

      val left = tag.tag
      val right = Tag[H1].tag

      println(TagT[ApplePaymentProvider].tag.debug())
      println(left.debug())
      println(right.debug())

      assertChild(left, right)
    }

    "consider class member's this-prefix to be the defining template, not the most specific prefix from where the call is happening (deliberate omission of this for better ergonomics in cakes)" in {
      trait A {
        class X

        final val singleton1 = "bar"
        type S1 = singleton1.type

        val singleton2 = "bar"
        type S2 = singleton2.type

        val xa = Tag[X].tag

//        val s1a = Tag[S1] // class type required but String("bar") found error on 2.11
        val s1a = LTT[S1]
        val s1a1 = Tag[singleton1.type].tag

//        val s2a = Tag[S2]
        val s2a = LTT[S2]
        val s2a1 = Tag[singleton2.type].tag
      }

      trait B extends A {
        val xb = Tag[X].tag

//        val s1b = Tag[S1].tag
        val s1b = LTT[S1]
        val s1b1 = Tag[singleton1.type].tag

        val s2b = LTT[S2]
        val s2b1 = Tag[singleton2.type].tag
      }

      object B extends B

      assertSameStrict(B.xa, B.xb)
      assertSameStrict(B.s1a, B.s1b)
      assertSameStrict(B.s1a1, B.s1b1)
      assertSameStrict(B.s2a, B.s2b)
      assertSameStrict(B.s2a1, B.s2b1)

      assertSameStrict(Tag[A#X].tag, B.xa)

      assertSameStrict(B.s1b, B.s1a)
      assertSameStrict(B.s1a, B.s1a1)
      assertSameStrict(B.s1b, B.s1b1)
    }

    "Does NOT synthesize Tags for abstract types, but recursively summons Tag[this.Abstract]" in {
      // no tag synthesized, there's no Tag[Abstract] unless defined
      assertDoesNotCompile("Tag[Abstract]")
      locally {
        implicit val implicitTag: Tag[Abstract] = Tag[Abstract](Tag[Int].closestClass, Tag[Int].tag)
        val tag = Tag[Option[Abstract]]
        assertSameStrict(tag.tag.typeArgs.head, implicitTag.tag)
        assertSameStrict(tag.tag, TagK[Option].tag.combine(implicitTag.tag))
      }
    }

    "DOES synthesize Tags for abstract types (object X; X.T), does not summon Tag[X.T]" in {
      val realTag = Tag[Option[SomeObject.Abstract]]
      locally {
        implicit val implicitTag: Tag[SomeObject.Abstract] = Tag[SomeObject.Abstract](Tag[Int].closestClass, Tag[Int].tag)
        val tag = Tag[Option[SomeObject.Abstract]]
        assertDifferent(tag.tag.typeArgs.head, implicitTag.tag)
        assertDifferent(tag.tag, TagK[Option].tag.combine(implicitTag.tag))
        assertSameStrict(realTag.tag, tag.tag)
      }
    }

    "DOES synthesize Tags for abstract types (trait X; X#T), does not summon Tag[X#T]" in {
      val realTag = Tag[Option[SomeTrait#Abstract]]
      locally {
        implicit val implicitTag: Tag[SomeTrait#Abstract] = Tag[SomeTrait#Abstract](Tag[Int].closestClass, Tag[Int].tag)
        val tag = Tag[Option[SomeTrait#Abstract]]
        assertDifferent(tag.tag.typeArgs.head, implicitTag.tag)
        assertDifferent(tag.tag, TagK[Option].tag.combine(implicitTag.tag))
        assertSameStrict(realTag.tag, tag.tag)
      }
    }

    "DOES synthesize Tags for abstract types (val x; x.T), does not summon Tag[x.T]" in {
      val x = new SomeTrait {}
      val realTag = Tag[Option[x.Abstract]]
      locally {
        implicit val implicitTag: Tag[x.Abstract] = Tag[x.Abstract](Tag[Int].closestClass, Tag[Int].tag)
        val tag = Tag[Option[x.Abstract]]
        assertDifferent(tag.tag.typeArgs.head, implicitTag.tag)
        assertDifferent(tag.tag, TagK[Option].tag.combine(implicitTag.tag))
        assertSameStrict(realTag.tag, tag.tag)
      }
    }

    "Work for an abstract type with available TagK when obscured by empty refinement" in {
      def testTagK[F[_]: TagK, T: Tag] = Tag[F[T {}] {}]

      assert(testTagK[Set, Int].tag == fromRuntime[Set[Int]])
    }

    "Work for an abstract type with available TagK when TagK is requested through an explicit implicit" in {
      def testTagK[F[_], T: Tag](implicit ev: Tag.auto.T[F]) = {
        val _ = ev
        Tag[F[T {}] {}]
      }

      assert(testTagK[Set, Int].tag == fromRuntime[Set[Int]])
    }

    "Work for an abstract type with available TagKK" in {
      def t1[F[_, _]: TagKK, T: Tag, G: Tag] = Tag[F[T, G]]

      assert(t1[ZOBA[Int, *, *], Int, String].tag == fromRuntime[ZOBA[Int, Int, String]])
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
        t2[SharedTagTest.this.Z, SharedTagTest.this.Z, T1[
          ZOB[String, Int, Byte],
          String,
          String,
          String,
          String,
          List
        ], SharedTagTest.this.Z, XY, SharedTagTest.this.Z, YX, Either, SharedTagTest.this.Z, SharedTagTest.this.Z, SharedTagTest.this.Z].tag
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

    "regression test: https://github.com/zio/izumi-reflect/issues/293 assemble tag for Builder[B, Collection[B]]" in {
      def tag[B: Tag](implicit tag: Tag[java.util.Collection[B]]) = Tag[mutable.Builder[B, java.util.Collection[B]]]

      assertSameStrict(tag[Int].tag, Tag[mutable.Builder[Int, java.util.Collection[Int]]].tag)
    }

    "combine intersection types" in {
      def t1[A: Tag] = Tag[String with A]
      def t2[A: Tag, B: Tag] = Tag[A with B]

      assertSameStrict(t1[Int].tag, Tag[Int with String].tag)
      assertSameStrict(t2[Int, String].tag, Tag[String with Int].tag)
      assertSameStrict(t1[String].tag, Tag[String].tag)
      assertSameStrict(t2[String, String].tag, Tag[String].tag)
    }

    "summon HKT Tag for a Java type" in {
      assertCompiles("TagK[java.util.Collection]")
    }

    "regression test: https://github.com/zio/izumi-reflect/issues/76 derive tag for a parametric trait inside object" in {
      assertSameStrict(X76.x.tag, Tag[X76.T[Int]].tag)
    }

    "this.type tags should be generated, but are identical with their class / object tag" in {
      val classTag = Tag[ThisPrefixTest.ThisPrefix].tag
      val objectTag = Tag[ThisPrefixTest.ThisPrefix.type].tag
      val classThisTag = new ThisPrefixTest.ThisPrefix().tag
      val objectThisTag = ThisPrefixTest.ThisPrefix.tag

      assertDebugSame(classThisTag, classTag)
      assertDebugSame(objectThisTag, objectTag)

      assertNotChildStrict(classTag, objectTag)
      assertNotChildStrict(classTag, objectThisTag)
      assertNotChildStrict(classThisTag, objectTag)
      assertNotChildStrict(classThisTag, objectThisTag)
    }

    "this.type should have correct prefix" in {
      val classTag = Tag[ThisPrefixTest.ThisPrefix].tag
      val objectTag = Tag[ThisPrefixTest.ThisPrefix.type].tag
      val classThisTag = new ThisPrefixTest.ThisPrefix().tag
      val objectThisTag = ThisPrefixTest.ThisPrefix.tag

      assert(classTag.ref.getPrefix.isDefined)
      assert(objectTag.ref.getPrefix.isDefined)
      assert(classThisTag.ref.getPrefix.isDefined)
      assert(objectThisTag.ref.getPrefix.isDefined)

      assert(classTag.ref.getPrefix == objectTag.ref.getPrefix)
      assert(classTag.ref.getPrefix == classThisTag.ref.getPrefix)
      assert(classTag.ref.getPrefix == objectThisTag.ref.getPrefix)
    }

    "regression test: https://github.com/zio/izumi-reflect/issues/83, convert trifunctor tag to bifunctor tag" in {
      import TestModel._
      def direct[F[+_, +_]: TagKK] = Tag[BIO2[F]]
      def indirectFrom3[F[-_, +_, +_]: TagK3] = direct[F[Any, +*, +*]]
      assertSame(direct[ZIO[Any, +*, +*]].tag, indirectFrom3[ZIO].tag)
    }

    "resolve TagK from TagKK" in {
      def getTag[F[+_, +_]: TagKK] = TagK[F[Throwable, *]]
      val tagEitherThrowable = getTag[Either].tag
      val tag = TagK[Either[Throwable, *]].tag

      assertSameStrict(tagEitherThrowable, tag)
      assertChildStrict(tagEitherThrowable, TagK[Either[Any, *]].tag)
      assertChildStrict(TagK[Either[Nothing, *]].tag, tagEitherThrowable)
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
      trait S[T] {
        val param: T
      }

      def reproduce[T: Tag]: ZLayer[Any, Nothing, Has[S[T]]] = {
        f[T]
          .map(
            p =>
              new S[T] {
                override val param: T = p
              }
          ).toLayer
      }

      assert(reproduce[Unit].t.tag == Tag[Has[S[Unit]]].tag)
    }

    "equal path-dependent tags for singleton types are expected to be equal" in {
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

    "return expected class tag" in {
      assert(Tag[List[_] with Set[_]].closestClass eq classOf[scala.collection.immutable.Iterable[_]])
      assert(!Tag[List[_] with Set[_]].hasPreciseClass)

      assert(Tag[AnyVal].closestClass eq classOf[AnyVal])
      assert(!Tag[AnyVal].hasPreciseClass)

      assert(Tag[String].closestClass ne classOf[AnyVal])
      assert(!Tag[String with Int].hasPreciseClass)

      assert(Tag[List[Int]].closestClass eq classOf[List[_]])
      assert(Tag[List[Int]].hasPreciseClass)
      assert(Tag[H1].hasPreciseClass)

      assert(Tag[ZY#T].closestClass eq classOf[Any])
      assert(!Tag[ZY#T].hasPreciseClass)

      assert(Tag[ZY#Y].hasPreciseClass)
    }

    "Work with term type prefixes" in {
      val zy = new ZY {}
      val zx = new ZY {}

      assertSameStrict(Tag[zy.T].tag, LTT[zy.T])
      assertNotChildStrict(Tag[zy.T].tag, LTT[zx.T])
      assertSameStrict(Tag[zy.x.type].tag, LTT[zy.x.type])
      assertChild(Tag[zy.x.type].tag, LTT[String])
      assertChild(Tag[zy.x.type].tag, LTT[java.io.Serializable])
      assertNotChildStrict(Tag[zy.x.type].tag, LTT[zx.x.type])
      assertSameStrict(Tag[zy.y.type].tag, LTT[zy.y.type])
      assertChild(Tag[zy.y.type].tag, LTT[java.lang.Object])
      assertNotChildStrict(Tag[zy.y.type].tag, LTT[zx.y.type])
      assertNotChildStrict(Tag[zy.y.type].tag, LTT[zx.x.type])
    }

    "correctly resolve abstract types inside traits when summoned inside trait" in {
      val a = new ContainerDef {}
      val b = new ContainerDef {}

      assert(a.make.tag == Tag[DockerContainer[a.T]].tag)
      assertDifferent(a.make.tag, Tag[DockerContainer[b.T]].tag)
      assertSameStrict(Tag[DockerContainer[a.T]].tag, Tag[DockerContainer[a.T]].tag)
      assertDifferent(Tag[DockerContainer[a.T]].tag, Tag[DockerContainer[b.T]].tag)

      val zy = new ZY {}
      assert(zy.tagT.getMessage contains "could not find implicit value")
      assert(zy.tagU.getMessage contains "could not find implicit value")
      assert(zy.tagV.getMessage contains "could not find implicit value")
      assert(zy.tagA.isSuccess)
    }

    "combine higher-kinded type lambdas without losing ignored type arguments" in {
      val tag = `LTT[_[+_,+_]]`[({ type l[F[+_, +_]] = BlockingIO3[λ[(`-R`, `+E`, `+A`) => F[E, A]]] })#l]
      val res = tag.combine(`LTT[_,_]`[IO])
      val tagMono = LTT[BlockingIO[IO]]
      assertSameStrict(res, tagMono)
    }

    "resolve a higher-kinded type inside a named type lambda with ignored type arguments" in {
      def mk[F[+_, +_]: TagKK] = Tag[BlockingIO3[F2To3[F, *, *, *]]]
      val tag = mk[IO]
      val tagMono = Tag[BlockingIO[IO]]
      assertSameStrict(tag.tag, tagMono.tag)
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
      assertSameStrict(combinedTag, expectedTag)
    }

    "correctly resolve a higher-kinded nested type inside a named swap type lambda" in {
      def mk[F[+_, +_]: TagKK] = Tag[BIOService[SwapF2[F, *, *]]]

      val tag = mk[Either]

      assertSameStrict(tag.tag, Tag[BIOService[SwapF2[Either, *, *]]].tag)
      assertSameStrict(tag.tag, Tag[BIOService[Swap]].tag)
      assertSameStrict(tag.tag, Tag[BIOService[λ[(E, A) => Either[A, E]]]].tag)
    }

    "support subtyping of parents parameterized with type lambdas in combined tags" in {
      val childBase = `LTT[_[_,_]]`[RoleChild]
      val childArg = `LTT[_,_]`[Either]
      val combinedTag = childBase.combine(childArg)
      val parentTag = LTT[RoleParent[Either[Throwable, *]]]
      val childTag = LTT[RoleChild[Either]]

      assertChild(combinedTag, childTag)
      assertSame(combinedTag, childTag)

      assertChild(combinedTag, parentTag)
      assertNotChild(parentTag, combinedTag)
    }

    "support subtyping of parents parameterized with type lambdas in combined tags with multiple parameters" in {
      val childBase = `LTT[_[+_,+_],_,_]`[RoleChild2]
      val childArgs = Seq(`LTT[_,_]`[Either], LTT[Int], LTT[String])
      val combinedTag = childBase.combine(childArgs: _*)
      val expectedTag = LTT[RoleParent[Either[Throwable, *]]]
      val noncombinedTag = LTT[RoleChild2[Either, Int, String]]

      assertSame(combinedTag, noncombinedTag)
      assertChild(noncombinedTag, expectedTag)
      assertChild(combinedTag, expectedTag)
    }

    "combine inside type lambdas with repeated usages of a type lambda type parameter" in {
      def mk[F[+_, +_]: TagKK] = Tag[RepeatedBlockingIO[F]]

      val tag = mk[IO]
      val tagMono = Tag[RepeatedBlockingIO[IO]]

      assertSameStrict(tag.tag, tagMono.tag)
    }

    "combine inside type lambdas with repeated usages of an outer type" in {
      def mk[F[+_, +_]: TagKK] = Tag[RepeatedNonLambdaBlockingIO[F]]

      val tag = mk[IO]
      val tagMono = Tag[RepeatedNonLambdaBlockingIO[IO]]

      assertSameStrict(tag.tag, tagMono.tag)
    }

    "combine inside type lambdas with repeated usages of an outer distinct type with the same type symbol" in {
      def mk[F[+_, +_]: TagKK] = Tag[RepeatedSymbolNonLambdaBlockingIO[F]]

      val tag = mk[IO]
      val tagMono = Tag[RepeatedSymbolNonLambdaBlockingIO[IO]]

      assertSameStrict(tag.tag, tagMono.tag)
    }

    "combine inside type lambdas where the type constructor of the type lambda result is a type lambda type parameter" in {
      def mk[T: Tag] = Tag[LambdaParamCtorBlockingIOT[Int]]

      val tag = mk[Int]
      val tagMono = Tag[LambdaParamCtorBlockingIOT[Int]]

      assertSameStrict(tag.tag, tagMono.tag)
    }

    "regression test: https://github.com/zio/izumi-reflect/issues/82, convert trifunctor hkt to bifunctor when combining tags" in {
      def tag[F[-_, +_, +_]: TagK3] = Tag[BIO2[F[Any, +*, +*]]]

      assertSameStrict(tag[ZIO].tag, Tag[BIO2[IO]].tag)
    }

    "combine higher-kinded types without losing ignored type arguments" in {
      def mk[F[+_, +_]: TagKK] = Tag[BlockingIO[F]]

      val tag = mk[IO]
      val tagMono = Tag[BlockingIO[IO]]

      assertSameStrict(tag.tag, tagMono.tag)
    }

    "resolve a higher-kinded type inside an anonymous type lambda with ignored & higher-kinded type arguments" in {
      def mk[F[_[_], _]: TagTK] = Tag[BlockingIO3T[({ type l[R, E[_], A] = F[E, A] })#l]]

      val tag = mk[OptionT]

      assertSameStrict(tag.tag, Tag[BlockingIOT[OptionT]].tag)
    }

    "correctly resolve a higher-kinded nested type inside an anonymous swap type lambda" in {
      def mk[F[+_, +_]: TagKK] = Tag[BIOService[λ[(E, A) => F[A, E]]]]

      val tag = mk[Either]

      assertSameStrict(tag.tag, Tag[BIOService[SwapF2[Either, *, *]]].tag)
      assertSameStrict(tag.tag, Tag[BIOService[Swap]].tag)
      assertSameStrict(tag.tag, Tag[BIOService[λ[(E, A) => Either[A, E]]]].tag)
    }

    "handles abstract types instead of parameters" in {
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

      assertSameStrict(t1.x.tag, fromRuntime[OptionT[List, Either[Int, Byte]]])
    }

    "Generates lambda parents for lambda bases" in {
      val childBase = `LTT[_[_,_]]`[RoleChild]

      val fullDb = childBase.basesdb
      fullDb.foreach {
        case (_, parents) =>
          parents.foreach {
            p =>
              if (p.toString.contains("RoleParent")) {
                assert(p.isInstanceOf[LightTypeTagRef.Lambda])
                assert(p.asInstanceOf[LightTypeTagRef.Lambda].input.size == 1)
              }
          }
      }
    }

    "progression test: subtyping for Invariant Java HKT doesn't work on Dotty" in {
      val collection = TagK[java.util.Collection]
      val javaIterable = TagK[java.lang.Iterable]
      assertChildStrict(collection.tag, javaIterable.tag)
    }

    "progression test: subtyping for Invariant Scala HKT doesn't work on Dotty" in {
      val mutableSet = TagK[scala.collection.mutable.Set]
      val collectionSet = TagK[scala.collection.Set]
      assertChildStrict(mutableSet.tag, collectionSet.tag)
    }

  }

}

trait SomeTrait {
  type Abstract
}

object SomeObject {
  type Abstract
}

// https://github.com/scala/bug/issues/11139
final case class testTag3[F[_]: TagK]() {
  type X = OptionT[F, Int]
  val res = Tag[X].tag
}

object X76 {
  sealed trait T[A]
  val x = implicitly[Tag[T[Int]]]
}
