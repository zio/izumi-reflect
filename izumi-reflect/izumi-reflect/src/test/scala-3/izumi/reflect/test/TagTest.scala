package izumi.reflect.test

import izumi.reflect.macrortti.LTag
import izumi.reflect.{HKTag, Tag, TagK, TagTK}
import izumi.reflect.test.ID.*
import izumi.reflect.test.PlatformSpecific.fromRuntime

class TagTest extends SharedTagTest with TagAssertions {

  override final val tagZ = Tag[String]

  trait Trait1 {
    def dep: Dep
  }
  trait Trait3[T <: Dep] extends Trait1 {
    def dep: T
  }

  "Tag (Dotty)" should {

    "Support identity lambda type equality" in {
      val idTpeLTT = TagK[Identity].tag
      val idLambdaLTT = TagK[[A] =>> A].tag
      assertSame(idTpeLTT, idLambdaLTT)
    }

    "Support union subtyping" in {
      trait Animal
      trait Dog extends Animal
      assertChild(Tag[Dog].tag, Tag[Animal].tag)
      assertChild(Tag[Dog | String].tag, Tag[Animal | String].tag)
      assertNotChild(Tag[Animal | String].tag, Tag[Dog | String].tag)
    }

    "Support HKTag for unapplied type lambdas with type bounds" in {
      trait X
      trait XAble[A <: X]
      class Y extends X

      def getTag[F[_ <: X]: Tag.auto.T] = Tag[F[Y]]

      assertSame(getTag[XAble].tag, Tag[XAble[Y]].tag)
    }

    "Handle Tags outside of a predefined set (Scala 3 Syntax, BUT!! This should work on Scala 2 as written - something is wrong with Scala 2 Tag.auto.T)" in {
      type TagX[T[_, _, _[_[_], _], _[_], _]] = Tag.auto.T[T]

      def testTagX[F[_, _, _[_[_], _], _[_], _]: TagX, A: Tag, B: Tag, C[_[_], _]: TagTK, D[_]: TagK, E: Tag] = Tag[F[A, B, C, D, E]]

      val value = testTagX[T2, Int, String, OptionT, List, Boolean]
      assert(value.tag == fromRuntime[T2[Int, String, OptionT, List, Boolean]])
    }

    "Can create custom type tags to support bounded generics, e.g. <: Dep in TagK (Scala 3 Syntax)" in {
      type `TagK<:Dep`[K[_ <: Dep]] = Tag[K]

      implicitly[`TagK<:Dep`[Trait3]].tag.withoutArgs =:= LTag[Trait3[Nothing]].tag.withoutArgs
    }

    "combine union types" in {
      def t1[A: Tag] = Tag[String | A]
      def t2[A: Tag, B: Tag] = Tag[A | B]

      assertSameStrict(t1[Int].tag, Tag[Int | String].tag)
      assertSameStrict(t2[Int, String].tag, Tag[String | Int].tag)
      assertSameStrict(t1[String].tag, Tag[String].tag)
      assertSameStrict(t2[String, String].tag, Tag[String].tag)
    }

  }

}

trait Layer[A] {
  def tag: Tag[A]
}

object Layer {
  def succeed[A: Tag](value: => A): Layer[A] =
    new Layer[A] {
      override def tag: Tag[A] = implicitly
    }
}

// should compile
// Example from here: https://github.com/zio/zio/issues/6071
object CachedRefinedTypeExample {

  trait Animal
  trait Dog extends Animal

  trait Service {
    def animal: Animal
  }

  val layer =
    Layer.succeed {
      new Service {
        def animal: Dog = new Dog {}
      }
    }

}
