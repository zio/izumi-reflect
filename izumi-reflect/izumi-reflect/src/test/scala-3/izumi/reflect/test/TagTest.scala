package izumi.reflect.test

import izumi.reflect.{Tag, TagK}
import izumi.reflect.test.ID._

class TagTest extends SharedTagTest with TagAssertions {

  override final val tagZ = Tag[String]

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
