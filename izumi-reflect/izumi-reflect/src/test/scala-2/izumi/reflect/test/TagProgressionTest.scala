package izumi.reflect.test

import izumi.reflect.Tag

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

    "progression test: we may accidentally materialize tags for type parameters that are prefixes of type projections (situation is not expressible in dotty)" in {
      class Path {
        type Child
      }
      val path = new Path

      // A has no tag and the definition of `getTag` should not compile at all. It's a bug that it compiles
      def getTag[A <: Path]: Tag[A#Child] = Tag[A#Child]

      val directChildTag = Tag[Path#Child].tag // Path::Child
      val indirectChildTag = getTag[path.type].tag // A|<Nothing..Path>::Child

      assertDifferent(indirectChildTag, directChildTag)
      assertNotChild(directChildTag, indirectChildTag)
      assertNotChild(indirectChildTag, directChildTag)

      assert(indirectChildTag.toString == "A|<Nothing..Path>::Child")
    }

  }

}
