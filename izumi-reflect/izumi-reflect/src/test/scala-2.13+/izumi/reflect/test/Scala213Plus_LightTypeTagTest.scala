package izumi.reflect.test

import org.scalatest.wordspec.AnyWordSpec
import izumi.reflect.macrortti.LTT

class Scala213Plus_LightTypeTagTest extends AnyWordSpec with TagAssertions with TagProgressions {

  "lightweight type tags (2.13+)" should {
    "literal types behave in sane manner https://github.com/zio/izumi-reflect/issues/284" in {
      import izumi.reflect._

      val v: "a" = "a"

      type vt = v.type
      object xa {
        final val x = v
        final val y = x
      }

      assertSameStrict(Tag["a"].tag, Tag[vt].tag)
      assertSameStrict(Tag[xa.x.type].tag, Tag["a"].tag)
      assertSameStrict(Tag[xa.y.type].tag, Tag["a"].tag)
      assertDebugSame(Tag["a"].tag, Tag[vt].tag)
      assertDebugSame(Tag[xa.x.type].tag, Tag["a"].tag)
      assertDebugSame(Tag[xa.y.type].tag, Tag["a"].tag)
      assertChildStrict(Tag["a"].tag, Tag[String].tag)
      assertChildStrict(Tag[vt].tag, Tag[String].tag)
      assertChildStrict(Tag[xa.x.type].tag, Tag[String].tag)
      assertChildStrict(Tag[xa.y.type].tag, Tag[String].tag)
    }

    "support string constant types (Scala 2.13+ syntax)" in {
      assertDifferent(LTT["abc"], LTT[String])
      assertDifferent(LTT["abc"], LTT["cba"])
      assertSameStrict(LTT["abc"], LTT["abc"])
      assertChildStrict(LTT["abc"], LTT[String])
    }
  }

}
