package izumi.reflect.test

import izumi.reflect.Tag
import org.scalatest.exceptions.TestFailedException

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

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

    "progression test: type tags with bounds are not currently requested by the macro on Scala 2" in {
      val t = intercept[TestFailedException] {
        assertCompiles("""
        type `TagK<:Dep`[K[_ <: Dep]] = izumi.reflect.HKTag[ { type Arg[A <: Dep] = K[A] } ]

        def t[T[_ <: Dep]: `TagK<:Dep`, A <: Dep: Tag] = Tag[T[A]]

        assert(t[Trait3, Dep].tag == Tag[Trait3[Dep]].tag)
        """)
      }
      assert(t.message.get.contains("could not find implicit value"))
    }

  }

}
