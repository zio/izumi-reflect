package izumi.reflect.test

import izumi.reflect.macrortti._

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  import TestModel._

  "[progression] lightweight type tag (Dotty)" should {

    "progression test: fails to support complex type lambdas (Dotty syntax)" in {
      doesntWorkYetOnDotty {
        assertSame(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[[A, B] =>> FM2[(B, A)]])
      }
      doesntWorkYetOnDotty {
        assertSame(`LTT[_[_]]`[[K[_]] =>> NestedTL2[W1, W2, K]], `LTT[_[_]]`[[G[_]] =>> FM2[G[S[W2, W1]]]])
      }
      assertChild(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[[A, B] =>> FM2[(B, A)]])
    }

    "progression test: fails to combine higher-kinded type lambdas without losing ignored type arguments (Dotty syntax)" in {
      val tag = `LTT[_[+_,+_]]`[[F[+_, +_]] =>> BlockingIO3[[R, E, A] =>> F[E, A]]]
      val res = tag.combine(`LTT[_,_]`[IO])
      doesntWorkYetOnDotty {
        assertSame(res, LTT[BlockingIO[IO]])
      }
    }

    "progression test: tautological intersections with Matchable are still preserved in internal structure despite being useless" in {
      doesntWorkYet {
        assertDebugSame(LTT[Matchable with Option[String]], LTT[Option[String]])
      }
    }

  }

}
