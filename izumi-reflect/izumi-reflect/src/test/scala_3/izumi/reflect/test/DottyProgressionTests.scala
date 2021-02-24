package izumi.reflect.test

import izumi.reflect.macrortti._
import org.scalatest.exceptions.TestFailedException

/** Things that DO NOT work currently in Scala 3 version but work in Scala 2 version */
class DottyProgressionTests extends TagAssertions {

  import TestModel._

  "dotty version" should {

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
