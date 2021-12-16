package izumi.reflect.test

import izumi.reflect.macrortti.*

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  import TestModel._

  "[progression] lightweight type tag (Dotty)" should {
    "progression test: wildcards are not supported (wildcard=Any, historically due to behavior of .dealias on 2.12/13, see scala.reflect.internal.tpe.TypeMaps#ExistentialExtrapolation)" in {
      doesntWorkYetOnDotty { assertDifferent(LTT[Set[_]], LTT[Set[Any]]) }
      doesntWorkYetOnDotty {assertDifferent(LTT[List[_]], LTT[List[Any]]) }
      doesntWorkYetOnDotty {assertChild(LTT[Set[Int]], LTT[Set[_]]) }
      assertNotChild(LTT[Set[_]], LTT[Set[Int]])
      assertChild(LTT[List[Int]], LTT[List[_]])
      assertNotChild(LTT[List[_]], LTT[List[Int]])
      doesntWorkYetOnDotty {assertChild(LTT[Int => Int], LTT[_ => Int]) }
    }

    "progression test: wildcards with bounds are not supported (upper bound is the type, historically due to behavior of .dealias on 2.12/13, see scala.reflect.internal.tpe.TypeMaps#ExistentialExtrapolation)" in {
      doesntWorkYetOnDotty {assertDifferent(LTT[Option[W1]], LTT[Option[_ <: W1]])}
      doesntWorkYetOnDotty {assertDifferent(LTT[Option[H2]], LTT[Option[_ >: H4 <: H2]])}
      doesntWorkYetOnDotty {assertDifferent(LTT[Option[Any]], LTT[Option[_ >: H4]])}
    }


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

  }

}
