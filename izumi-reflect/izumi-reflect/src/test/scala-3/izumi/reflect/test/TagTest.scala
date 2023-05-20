package izumi.reflect.test

import izumi.reflect.macrortti.{LTT, LTag}
import izumi.reflect.{HKTag, Tag, TagK, TagTK}
import izumi.reflect.test.ID.*
import izumi.reflect.test.PlatformSpecific.fromRuntime
import izumi.reflect.test.TestModel.{Trait1, Trait3, Trait4}
import org.scalatest.exceptions.TestFailedException

class TagTest extends SharedTagTest with TagAssertions {

  override final val tagZ = Tag[String]

  trait Trait1 {
    def dep: Dep
  }
  trait Trait3[T <: Dep] extends Trait1 {
    def dep: T
  }

  "Tag (Dotty)" should {

    "Support union subtyping (Scala 3 specific, union types)" in {
      trait Animal
      trait Dog extends Animal
      assertChild(Tag[Dog].tag, Tag[Animal].tag)
      assertChild(Tag[Dog | String].tag, Tag[Animal | String].tag)
      assertNotChild(Tag[Animal | String].tag, Tag[Dog | String].tag)
    }

    "Support HKTag for unapplied type lambdas with type bounds (Scala 3 specific, union types)" in {
      trait X
      trait XAble[A <: X]
      class Y extends X

      def getTag[F[_ <: X]: Tag.auto.T] = Tag[F[Y]]

      assertSame(getTag[XAble].tag, Tag[XAble[Y]].tag)
    }

    "Can create custom type tags to support bounded generics, e.g. <: Dep in TagK (Scala 3 HKTag Syntax)" in {
      type `TagK<:Dep`[K[_ <: Dep]] = Tag[K]

      implicitly[`TagK<:Dep`[Trait3]].tag.withoutArgs =:= LTag[Trait3[Nothing]].tag.withoutArgs
    }

    "combine union types (Scala 3 specific, union types)" in {
      def t1[A: Tag] = Tag[String | A]
      def t2[A: Tag, B: Tag] = Tag[A | B]

      assertSameStrict(t1[Int].tag, Tag[Int | String].tag)
      assertSameStrict(t2[Int, String].tag, Tag[String | Int].tag)
      assertSameStrict(t1[String].tag, Tag[String].tag)
      assertSameStrict(t2[String, String].tag, Tag[String].tag)
    }

    "type tags with bounds are successfully requested by TagMacro" in {
      type `TagK<:Dep`[K[_ <: Dep]] = Tag.auto.T[K]

      def t[T[_ <: Dep]: `TagK<:Dep`, A <: Dep: Tag] = Tag[T[A]]

      assertSameStrict(t[Trait3, Dep].tag, Tag[Trait3[Dep]].tag)
    }

    "remove tautological unions with Nothing (LTT)" in {
      assertSameStrict(LTT[Nothing | Option[String]], LTT[Option[String]])
    }

    "remove tautological unions with Nothing (Tag)" in {
      assertSameStrict(Tag[Nothing | Option[String]].tag, LTT[Option[String]])
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
