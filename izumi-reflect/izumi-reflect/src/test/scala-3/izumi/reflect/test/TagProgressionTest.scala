package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.test.TestModel.*

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Dotty)" should {

    // Can't fix this atm because simplification happens even without .simplified call because of https://github.com/lampepfl/dotty/issues/17544
    "progression test: fails to don't lose tautological union components other than Nothing" in {
      def tag1[T: Tag]: Tag[T | Trait1] = Tag[T | Trait1]
      def tag4[T: Tag]: Tag[T | Trait4] = Tag[T | Trait4]

      val t1 = tag1[Trait3[Dep]].tag
      val t2 = tag4[Trait3[Dep]].tag

      val t10 = Tag[Trait3[Dep] | Trait1].tag
      val t20 = Tag[Trait3[Dep] | Trait4].tag

      brokenOnScala3 {
        assertSameStrict(t1, t10)
        assertDebugSame(t1, t10)
      }

      assertSameStrict(t2, t20)
      assertDebugSame(t2, t20)
    }

    // We don't really want to fix it, because removing tautologies is quadratic, with two subtyping comparisions per step!
    // Would make construction really expensive, all for an extremely rare corner case
    "progression test: union tautologies are not removed automatically when constructing combined union type" in {
      def tag1[T: Tag]: Tag[T | Trait1] = Tag[T | Trait1]

      val t1 = tag1[Trait3[Dep]].tag

      val t10 = t1.removeUnionTautologies

      brokenOnScala3 {
        assertSameStrict(t1, t10)
        assertDebugSame(t1, t10)
      }
    }

  }

}
