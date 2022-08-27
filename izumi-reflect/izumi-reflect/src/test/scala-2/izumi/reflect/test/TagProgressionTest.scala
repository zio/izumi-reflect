package izumi.reflect.test

import izumi.reflect.Tag

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

    "progression test: we may accidentally materialize tags for type parameters that are prefixes of type projections (Scala 2 specific, generic type projection)" in {
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

    "progression test: combine intersection path-dependent intersection types with inner tags doesn't work yet (Scala 2 specific, generic type projection)" in {
      trait PDT {
        type T
        implicit def tag: Tag[T]

        def badCombine(that: PDT): Tag[T with that.T] = {
          Tag[T with that.T]
        }
      }
      doesntWorkYet {
        assertCompiles(
          """
          trait PDT0 {
            type T
            implicit def tag: Tag[T]

            def goodCombine(that: PDT): Tag[T with that.T] = {
              import that.tag
              Tag[T with that.T]
            }
          }"""
        )
      }
      def PDT[U: Tag]: PDT = new PDT { type T = U; override val tag: Tag[U] = Tag[U] }

      val badCombine = PDT[Int].badCombine(PDT[Unit])
      doesntWorkYet {
        assertSameStrict(badCombine.tag, Tag[Int with Unit].tag)
      }
    }

  }

}
