package izumi.reflect.test

import izumi.reflect.{Tag, TagK}
import izumi.reflect.test.TestModel.RefinedTypeMemberModel._

class RefinedTypeMemberSubtypingTest extends TagAssertions {

  "Refined type member subtyping (issue #481)" should {

    "recognize concrete type member overrides against refined parent types" in {
      assertChildStrict(Tag[IntMember].tag, Tag[BaseWithMember { type T = Int }].tag)
      assertNotChild(Tag[IntMember].tag, Tag[BaseWithMember { type T = String }].tag)
      assertNotChild(Tag[BaseWithMember].tag, Tag[BaseWithMember { type T = Int }].tag)
    }

    "recognize concrete and bounded type members against upper-bounded refinements" in {
      assertChildStrict(Tag[IntMember].tag, Tag[BaseWithMember { type T <: AnyVal }].tag)
      assertNotChild(Tag[StringMember].tag, Tag[BaseWithMember { type T <: AnyVal }].tag)
      assertChildStrict(Tag[NarrowedBoundedMember].tag, Tag[BoundedBaseWithMember { type T <: AnyVal }].tag)
    }

    "preserve inherited type member overrides across subtraits" in {
      assertChildStrict(Tag[SubIntMember].tag, Tag[BaseWithMember { type T = Int }].tag)
      assertChildStrict(Tag[SubIntMember].tag, Tag[BaseWithMember { type T <: AnyVal }].tag)
      assertChildStrict(Tag[IntMember].tag, Tag[{ type T = Int }].tag)
    }

    "resolve type members instantiated via class type parameters and TagK combine" in {
      val combinedTag = TagK[ParameterizedMember].tag.combine(Tag[Int].tag)
      assertChildStrict(Tag[ParameterizedMember[Int]].tag, Tag[BaseWithMember { type T = Int }].tag)
      assertChildStrict(Tag[ParameterizedMember[Int]].tag, Tag[BaseWithMember { type T <: AnyVal }].tag)
      assertChildStrict(combinedTag, Tag[BaseWithMember { type T = Int }].tag)
    }

  }

}
