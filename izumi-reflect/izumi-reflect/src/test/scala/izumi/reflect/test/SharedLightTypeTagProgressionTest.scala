package izumi.reflect.test

import izumi.reflect.macrortti._

/**
  * The tests here are *progression* tests, that means they test that something *doesn't work*
  *
  * If a test here starts to fail that's a GOOD thing - that means a new feature is now supported.
  * When that happens you can remove the `broken` condition inversions and move the test to
  * the non-progression test suite.
  *
  * All tests must have `broken` clauses wrapping the expected GOOD conditions if a feature
  * were to work. If a test is missing `broken` clause, it's a probably not a progression test
  * anymore and should be moved.
  */
abstract class SharedLightTypeTagProgressionTest extends TagAssertions with TagProgressions {

  "[progression] lightweight type tags (all versions)" should {

    import TestModel._

    "progression test: can't support subtyping of type prefixes" in {
      val a = new C {}

      broken {
        assertChild(LTT[a.A], LTT[C#A])
      }
      assertDifferent(LTT[a.A], LTT[C#A])
      assertNotChild(LTT[C#A], LTT[a.A])
    }

    "progression test: can't support subtyping of concrete type projections" in {
      trait A {
        trait T
      }
      trait B extends A

      val tagA = LTT[A#T]
      val tagB = LTT[B#T]

      assertSame(LTT[A#T], LTT[A#T])
      assertDifferent(LTT[B#T], LTT[A#T])
      broken {
        assertChild(tagB, tagA)
      }
    }

    "progression test: bounds-based subtype checks for lambdas do not work properly (LambdaParameter must contain bounds and NameReferences shouldn't for this to work)" in {
      // I consider this stuff practically useless
      type X[A >: H4 <: H2] = Set[A]
      type X1[A >: H3 <: H3] = Set[A]
      type X2[A >: H5 <: H5] = Set[A]

//      def compare[a >: c <: b, b <: d, c <: d, d, A[x >: a <: b] <: B[x], B[_ >: c <: d], x >: a <: b](s: A[x], t: B[_ >: c <: d]) = null

      broken {
////      compare[H5, H5, H4, H2, X2, X, H5](null: Set[H5], null) // error
////      (null: Set[H5]): Set[_ >: H4 <: H2] // error
        assertNotChild(`LTT[A,B,_>:B<:A]`[H5, H5, X2], `LTT[A,B,_>:B<:A]`[H2, H4, X])
      }

//      compare[H3, H3, H4, H2, X1, X, H3](null: Set[H3], null)
//      (null: Set[H3]): Set[_ >: H4 <: H2]
      assertChild(`LTT[A,B,_>:B<:A]`[H3, H3, X1], `LTT[A,B,_>:B<:A]`[H2, H4, X])
    }

    "progression test: indirect structural checks do not work" in {
      assertDifferent(LTT[{ type A }], LTT[Object])
      assertChildStrict(LTT[C], LTT[{ type A }])
    }

    "progression test: combined intersection lambda tags still contain some junk bases (coming from the unsound same-arity assumption in LightTypeTag#combine)" in {
      val tCtor = `LTT[_,_]`[T3]
      val combined = tCtor.combine(LTT[Int], LTT[Boolean])
      val debugCombined = combined.debug("combined")

      val alias = LTT[T3[Int, Boolean]]
      val direct = LTT[W1 with W4[Boolean] with W5[Int]]

      broken {
        assert(!debugCombined.contains("W4[=scala.Int]"))
      }
      broken {
        assert(!debugCombined.contains("W3[=scala.Int]"))
      }

      broken {
        assertDebugSame(combined, alias)
      }
      broken {
        assertDebugSame(combined, direct)
      }
    }

    "progression test: combined lambda tags still contain some junk bases (coming from the unsound same-arity assumption in LightTypeTag#combine)" in {
      val curriedApplied = `LTT[_,_]`[Right].combine(LTT[Throwable]).combine(LTT[Unit])
      val debug1 = curriedApplied.debug()

      assertSame(curriedApplied, LTT[Right[Throwable, Unit]])

      assert(debug1.contains(": scala.util.Right[+java.lang.Throwable,+scala.Unit]"))
      assert(debug1.contains("- scala.util.Right[+java.lang.Throwable,+scala.Unit]"))
      assert(debug1.contains("* scala.Product"))
      assert(debug1.contains("- λ %1 → scala.util.Right[+java.lang.Throwable,+1]"))
      assert(debug1.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      broken {
        assert(!debug1.contains("λ %1 → scala.util.Right[+scala.Unit,+1]"))
      }
    }

    "progression test: Dotty fails to `support methods with type parameters in structural refinements`" in {
      trait X { def x[A](a: A): A }

      assertNotChildStrict(LTT[X { def x[A](a: A): Boolean }], LTT[X { def x[A](a: A): Int }])
      assertChildStrict(LTT[X { def x[A](a: A): Boolean }], LTT[X { def x[A](a: A): AnyVal }])
      broken {
        assertChildStrict(LTT[X { def x[A](a: A): Boolean }], LTT[X { def x[A](a: A): A }])
      }
    }

    "fails to treat tautological refinements as equal to the underlying type" in {
      trait X { def x[A](a: A): A }

      broken {
        assertSameStrict(LTT[X], LTT[X { def x[A](a: A): A }])
        assertSameStrict(LTT[Object], LTT[Object { def equals(obj: Object): Boolean }])
      }
    }

    "fails to support methods with multiple parameter lists in refinements" in {
      brokenOnScala2 {
        assertNotChildStrict(LTT[{ def x(i: Int)(b: Boolean): Int }], LTT[{ def x(b: Boolean): Int }])
      }
      brokenOnScala2 {
        assertNotChildStrict(LTT[{ def x(i: Int)(b: Boolean): Int }], LTT[{ def x(i: Int): Int }])
      }

      brokenOnScala3 {
        // no distinction between method with multiple param lists and one param list
        assertNotChildStrict(LTT[{ def x(i: Int)(b: Boolean): Int }], LTT[{ def x(i: Int, b: Boolean): Int }])
      }

      assertChildStrict(LTT[{ def x(i: Int)(b: Boolean): Int }], LTT[{ def x(i: Int)(b: Boolean): Any }])
      // This isn't quite true according to Scalac:
      //   implicitly[({ def x(i: Int)(b: Boolean): Int }) <:< ({ def x(i: Int)(b: Nothing): Int })] // false
      // But since the equivalent with values instead of methods is true, we regard this as true:
      //   implicitly[({ def x(i: Int): Boolean => Int }) <:< ({ def x(i: Int): Nothing => Int })] // true
      assertChildStrict(LTT[{ def x(i: Int)(b: Boolean): Int }], LTT[{ def x(i: Int)(b: Nothing): Int }])
    }

    "progression test: fails to `Any/Object relation is consistent with Scala`" in {
      implicitly[Object <:< Any]
      def isNoImplicit[A <: AnyRef](implicit ev: A = null): Boolean = ev eq null
      assert(isNoImplicit[Any <:< Object])

      assertChild(LTT[Object], LTT[Any])
      broken {
        assertNotChild(LTT[Any], LTT[Object])
      }
      assertDifferent(LTT[Any], LTT[Object])
    }

    "progression test: can't distinguish between equal-bounded type and alias inside refinements on dotty" in {
      val t4 = LTT[{ type X >: Any <: Any }] // (java.lang.Object {type X = Any}), but should be (java.lang.Object {type X::<Any..Any>})
      val t5 = LTT[{ type X = Any }]

      brokenOnScala3 {
        assertDifferent(t4, t5)
      }
    }

    "progression test: Null type is a subtype of Nothing, but shouldn't be" in {
      broken {
        assertNotChild(LTT[Null], LTT[Nothing])
      }
    }

  }

}
