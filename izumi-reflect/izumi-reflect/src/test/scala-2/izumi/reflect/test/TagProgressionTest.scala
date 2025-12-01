package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.test.TestModel._

class TagProgressionTest extends SharedTagProgressionTest {

  "[progression] Tag (Scala 2)" should {

    "progression test: type tags with bounds ARE now supported by the macro on Scala 2 (Issue #30 fix)" in {
      // This test documents that Issue #30 has been fixed:
      // HKTag with type bounds now works on Scala 2.
      // Previously this was a known limitation that would fail with "could not find implicit value".
      type `TagK<:Dep`[K[_ <: Dep]] = izumi.reflect.HKTag[{ type Arg[A <: Dep] = K[A] }]

      def t[T[_ <: Dep]: `TagK<:Dep`, A <: Dep: Tag] = Tag[T[A]]

      assert(t[Trait3, Dep].tag == Tag[Trait3[Dep]].tag)
    }

  }

}
