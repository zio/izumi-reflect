package izumi.reflect.test

import izumi.reflect.Tag
import org.scalatest.wordspec.AnyWordSpec

class Repro481Test extends AnyWordSpec {
  "LightTypeTag" should {
    "correctly handle subtype relationship for refined types" in {
      trait A {
        type T
      }

      trait AInt extends A {
        override type T = Int
      }

      val tagAInt = Tag[AInt].tag
      val tagRefinement = Tag[A { type T = Int }].tag
      
      println(s"Tag[AInt]: $tagAInt")
      println(s"Tag[A { type T = Int }]: $tagRefinement")
      
      assert(tagAInt <:< tagRefinement)
    }
  }
}
