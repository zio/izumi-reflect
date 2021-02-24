package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.macrortti._
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, Boundaries}

import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

class DottyProgressionTests extends TagAssertions {

  import TestModel._

  "dotty version" should {

    "does not fail on unresolved type parameters" in {
//      def badTag[T]: Tag[T] = izumi.reflect.Tag.tagFromTagMacro[T]
      assertCompiles(
        """
        def badTag[T]: Tag[T] = Tag[T]
        """)
    }

    "does not fail on intersection/union of unresolved type parameters" in {
      assertCompiles(
              """
              def badTag[T, U]: Tag[T & U] = Tag[T & U]
              """)
      assertCompiles(
              """
              def badTag[T, U]: Tag[T | U] = Tag[T | U]
              """)
    }

  }

}
