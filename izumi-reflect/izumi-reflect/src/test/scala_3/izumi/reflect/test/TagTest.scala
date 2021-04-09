package izumi.reflect.test

import izumi.reflect.{Tag, TagK}
import izumi.reflect.test.ID._

class TagTest extends SharedTagTest {

  override final val tagZ = Tag[String]

  "Tag" should {

    "Support identity lambda type equality" in {
      val idTpeLTT = TagK[Identity].tag
      val idLambdaLTT = TagK[[A] =>> A].tag
      assert(idTpeLTT == idLambdaLTT)
    }

  }

}
