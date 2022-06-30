package izumi.reflect.test

import izumi.reflect._
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag210
import izumi.reflect.macrortti.{LTT, LTag, LightTypeTag}
import izumi.reflect.test.ID._
import izumi.reflect.test.PlatformSpecific.fromRuntime
import izumi.reflect.test.TestModel.{ApplePaymentProvider, H1, IdAnnotation, T1}
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import org.scalatest.Assertions
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

import java.util
import scala.annotation.StaticAnnotation
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

  val tagT = intercept[TestFailedException](assertCompiles("Tag[T]"))
  val tagU = intercept[TestFailedException](assertCompiles("Tag[U]"))
  val tagV = intercept[TestFailedException](assertCompiles("Tag[V]"))
  val tagA = Try(assertCompiles("Tag[A]"))
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

trait T2[A, B, C[_[_], _], D[_], E]

abstract class SharedTagTest extends AnyWordSpec with XY[String] with TagAssertions with InheritedModelKindProjector {

  type Abstract
  type Swap[A, B] = Either[B, A]
  type SwapF2[F[_, _], A, B] = F[B, A]
  type Id[A] = A
  type Id1[F[_], A] = F[A]

  import izumi.reflect.test.PlatformSpecific.fromRuntime

  def fromLTag[T: LTag]: LightTypeTag = LTag[T].tag

  final val str = "str"

  final class With[T] extends StaticAnnotation

  case class ZOBA[A, B, C](value: Either[B, C])

  trait Test[A, dafg, adfg, LS, L[_], SD, GG[A] <: L[A], ZZZ[_, _], S, SDD, TG]
  trait T1[A, B, C, D, E, F[_]]
  trait YX[V] extends XY[V]

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
    }

    "Tag.auto.T kind inference macro works for known cases" in {
      def x[T[_]: Tag.auto.T]: TagK[T] = implicitly[Tag.auto.T[T]]

      def x2[T[_, _]: Tag.auto.T]: TagKK[T] = implicitly[Tag.auto.T[T]]

      def x3[T[_, _, _[_[_], _], _[_], _]](implicit x: Tag.auto.T[T]): Tag.auto.T[T] = x

      val b1 = x[Option].tag =:= TagK[Option].tag
      val b2 = x2[Either].tag =:= TagKK[Either].tag
      val b3 = implicitly[Tag.auto.T[OptionT]].tag =:= TagTK[OptionT].tag
      val b4 = x3[T2].tag.withoutArgs =:= LTag[T2[Nothing, Nothing, Nothing, Nothing, Nothing]].tag.withoutArgs

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

      assertSame(B.xa, B.xb)
      assertSame(B.s1a, B.s1b)
      assertSame(B.s1a1, B.s1b1)
      assertSame(B.s2a, B.s2b)
      assertSame(B.s2a1, B.s2b1)

      assertSame(Tag[A#X].tag, B.xa)

      assertSame(B.s1b, B.s1a)
      assertSame(B.s1a, B.s1a1)
      assertSame(B.s1b, B.s1b1)
    }

  }

  "Does not synthesize Tags for abstract types, but recursively summons (this.Abstract)" in {
    implicit val tag0: Tag[Abstract] = Tag.apply(Tag[Int].closestClass, Tag[Int].tag)
    val tag = Tag[Option[Abstract]]
    assert(tag.tag.typeArgs.head == tag0.tag)
  }

  "DOES synthesize Tags for abstract types (object X; X.T)" in {
    implicit val tag0: Tag[SomeObject.Abstract] = Tag.apply(Tag[Int].closestClass, Tag[Int].tag)
    val tag = Tag[Option[SomeObject.Abstract]]
    assert(tag.tag.typeArgs.head != tag0.tag)
  }

  "DOES synthesize Tags for abstract types (trait X; X#T)" in {
    implicit val tag0: Tag[SomeTrait#Abstract] = Tag.apply(Tag[Int].closestClass, Tag[Int].tag)
    val tag = Tag[Option[SomeTrait#Abstract]]
    assert(tag.tag.typeArgs.head != tag0.tag)
  }

  "DOES synthesize Tags for abstract types (val x; x.T)" in {
    val x = new SomeTrait {}
    implicit val tag0: Tag[x.Abstract] = Tag.apply(Tag[Int].closestClass, Tag[Int].tag)
    val tag = Tag[Option[x.Abstract]]
    assert(tag.tag.typeArgs.head != tag0.tag)
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

    assertDeepSame(tag[Int].tag, Tag[mutable.Builder[Int, java.util.Collection[Int]]].tag)
  }

  "combine intersection types" in {
    def t1[A: Tag] = Tag[String with A]
    def t2[A: Tag, B: Tag] = Tag[A with B]

    assertDeepSame(t1[Int].tag, Tag[Int with String].tag)
    assertDeepSame(t2[Int, String].tag, Tag[String with Int].tag)
    assertDeepSame(t1[String].tag, Tag[String].tag)
    assertDeepSame(t2[String, String].tag, Tag[String].tag)
  }

  "summon HKT Tag for a Java type" in {
    assertCompiles("TagK[java.util.Collection]")
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
