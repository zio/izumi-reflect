package izumi.reflect.test

import izumi.reflect.macrortti._

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  "[progression] lightweight type tag (Dotty)" should {

    "fails to support variance for type parameters of opaque types" in {
      object x {
        opaque type T[+X] = X
      }

      broken {
        assertChildStrict(LTT[x.T[Int]], LTT[x.T[AnyVal]])
      }
    }

  }

}
