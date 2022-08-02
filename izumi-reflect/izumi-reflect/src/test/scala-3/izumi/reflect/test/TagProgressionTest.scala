package izumi.reflect.test

import izumi.reflect.Tag

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

    "progression test: combine intersection path-dependent intersection types with inner tags doesn't work yet (Scala 3)" in {
      trait PDT {
        type T
        implicit def tag: Tag[T]

        def badCombine(that: PDT): Tag[T with that.T] = {
          Tag[T with that.T]
        }
        def goodCombine(that: PDT): Tag[T with that.T] = {
          import that.tag
          Tag[T with that.T]
        }
      }
      def PDT[U: Tag]: PDT = new PDT { type T = U; override val tag: Tag[U] = Tag[U] }

      val badCombine = PDT[Int].badCombine(PDT[Unit])
      doesntWorkYet {
        assertSameStrict(badCombine.tag, Tag[Int with Unit].tag)
      }

      val goodCombine = PDT[Int].badCombine(PDT[Unit])
      doesntWorkYet {
        assertSameStrict(goodCombine.tag, Tag[Int with Unit].tag)
      }
    }

  }

}
