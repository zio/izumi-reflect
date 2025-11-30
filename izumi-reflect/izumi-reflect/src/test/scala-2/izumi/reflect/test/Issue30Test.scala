package izumi.reflect.test

import izumi.reflect._
import org.scalatest.wordspec.AnyWordSpec

class Issue30Test extends AnyWordSpec {
  "Issue 30" should {
    "support HKTag for unapplied type lambdas with type bounds" in {
      trait X
      trait XAble[_ <: X]
      class Y extends X

      def getTag[F[_ <: X]: Tag.auto.T] = {
        val ev = implicitly[Tag.auto.T[F]]
        Tag[F[Y]]
      }

      val tag = getTag[XAble]
      assert(tag.tag == Tag[XAble[Y]].tag)
    }
  }
}
