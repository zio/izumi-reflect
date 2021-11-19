package izumi.reflect.test

import izumi.reflect.{Tag, TagK}
import izumi.reflect.test.ID._

class TagTest extends SharedTagTest with TagAssertions {

  override final val tagZ = Tag[String]

  "Tag" should {

    "Support identity lambda type equality" in {
      val idTpeLTT = TagK[Identity].tag
      val idLambdaLTT = TagK[[A] =>> A].tag
      assert(idTpeLTT == idLambdaLTT)
    }

    "Support union subtyping" in {
      trait Animal
      trait Dog extends Animal
      assertChild(Tag[Dog].tag, Tag[Animal].tag)
      assertChild(Tag[Dog | String].tag, Tag[Animal | String].tag)
      assertNotChild(Tag[Animal | String].tag, Tag[Dog | String].tag)
    }
  }

}
