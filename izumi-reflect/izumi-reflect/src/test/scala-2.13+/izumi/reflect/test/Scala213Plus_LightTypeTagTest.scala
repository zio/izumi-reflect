package izumi.reflect.test

import org.scalatest.wordspec.AnyWordSpec
import izumi.reflect.macrortti.LTT

class Scala213Plus_LightTypeTagTest extends AnyWordSpec with TagAssertions with TagProgressions {

  "lightweight type tags (2.13+)" should {
    "literal types behave in sane manner https://github.com/zio/izumi-reflect/issues/284" in {
      import izumi.reflect._

      val v: "a" = "a"
      type vt = v.type

      assertChild(Tag["a"].tag, Tag[vt].tag)
      assertChild(Tag[vt].tag, Tag["a"].tag)
      assertSame(Tag[vt].tag, Tag["a"].tag)
      assertSame(Tag["a"].tag, Tag["a"].tag)
      assertSame(Tag[vt].tag, Tag[vt].tag)
      assertChild(Tag["a"].tag, Tag[String].tag)
      assertChild(Tag[vt].tag, Tag[String].tag)
      assertNotChild(Tag[String].tag, Tag["a"].tag)
      assertNotChild(Tag[String].tag, Tag[vt].tag)
    }

    "support string constant types (Scala 2.13+ syntax)" in {
      assertDifferent(LTT["abc"], LTT[String])
      assertDifferent(LTT["abc"], LTT["cba"])
      assertSameStrict(LTT["abc"], LTT["abc"])
      assertChildStrict(LTT["abc"], LTT[String])
    }
  }

}
