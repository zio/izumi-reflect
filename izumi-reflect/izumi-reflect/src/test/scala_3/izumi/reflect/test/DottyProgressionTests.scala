package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.macrortti._
import org.scalatest.exceptions.TestFailedException

/** Things that DO NOT work correctly in Scala 3 version but work in Scala 2 version */
class DottyProgressionTests extends TagAssertions {

  import TestModel._

  "dotty version" should {

    // "does not fail on unresolved type parameters" in {
    //   def badTag[T]: Tag[T] = Tag[T]
    // }

    // "does not fail on intersection/union of unresolved type parameters" in {
    //   def badTag0[R, R0] = Tag[R with R0]
    //   def badTag1[T, U]: Tag[T & U] = Tag[T & U]
    //   def badTag2[T, U]: Tag[T | U] = Tag[T | U]
    // }

    "fails to check subtyping when higher-kinds are involved" in {
      intercept[TestFailedException] {
        assertChild(LTT[FT2[IT2]], LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertSame(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
      }
    }

    "fails to combine higher-kinded type lambdas without losing ignored type arguments" in {
      intercept[TestFailedException] {
        val tag = `LTT[_[+_,+_]]`[[F[+_, +_]] =>> BlockingIO3[[R, E, A] =>> F[E, A]]]

        val res = tag.combine(`LTT[_,_]`[IO])
        assert(res == LTT[BlockingIO[IO]])
      }
    }

  }

}
