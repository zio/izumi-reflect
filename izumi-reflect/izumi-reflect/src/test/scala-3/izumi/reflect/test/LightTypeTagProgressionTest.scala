package izumi.reflect.test

import izumi.reflect.macrortti._

class LightTypeTagProgressionTest extends SharedLightTypeTagProgressionTest {

  import TestModel._

  "[progression] lightweight type tag (Dotty)" should {

    "fails to support polymorphic function types" in {
//      val t1 = LTT[[A] => A => A]
//      val t2 = LTT[[B] => B => B]
//      assertSameStrict(t1, t2)
    }

  }

}
