package izumi.reflect.test

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.{Tag, TagK, TagK3}

object TagMacroTest {
  trait P
  trait C extends P
  trait HuIO[-R, +E, +I]

  class Case1[T] {
    final def addHas[F[-_, +_, +_]: TagK3, E: Tag, I <: T: Tag](): LightTypeTag = {
      val t1 = TagK[F[Any, E, *]]
      val t2 = t1.tag.combine(Tag[I].tag)
      t2
    }
  }

}

class TagMacroTest extends TagAssertions {
  import TagMacroTest._
  "Tag macro" should {
    "reconstruct lambda tags" in {
      val expected = Tag[HuIO[Any, Int, C]].tag
      val combined = TagK[HuIO[Any, Int, *]].tag.combine(Tag[C].tag)
      assert(combined == expected)
      val returned = new Case1[P]().addHas[HuIO, Int, C]()
      assert(returned == expected)
    }
  }
}
