package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.test.TestModel._
import izumi.reflect._
import izumi.reflect.test.PlatformSpecific.fromRuntime
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

abstract class SharedTagProgressionTest extends AnyWordSpec with TagAssertions with TagProgressions with InheritedModel {

  "[progression] Tag (all versions)" should {

    "progression test: can't substitute type parameters inside defs/vals in structural types" in {
      def t1[T: Tag]: Tag[{ def x: T }] = Tag[{ def x: T }]
      def t2[T: Tag]: Tag[{ val x: T }] = Tag[{ val x: T }]

      doesntWorkYet {
        assertSameStrict(t1[Int].tag, Tag[{ def x: Int }].tag)
      }
      doesntWorkYet {
        assertSameStrict(t2[Int].tag, Tag[{ val x: Int }].tag)
      }
    }

    "progression test: cannot resolve a higher-kinded type in a higher-kinded tag in a named deeply-nested type lambda on Scala 2" in {
      val t = Try(intercept[TestFailedException] {
        assertCompiles(
          """
    def mk[F[+_, +_]: TagKK] = TagKK[({ type l[A, B] = BIOServiceL[F, A, B] })#l]
    val tag = mk[Either]

    assert(tag.tag == LTagKK[({ type l[E, A] = BIOService[ ({ type l[X, Y] = Either[A, E] })#l ] })#l].tag)
    """
        )
      })
      doesntWorkYetOnScala2 {
        assert(t.isFailure)
      }
      succeedsOnScala2ButShouldnt {
        assert(t.isSuccess)
        assert(
          t.get.getMessage.contains("could not find implicit value") ||
          t.get.getMessage.contains("diverging implicit") /*2.11*/
        )
      }
    }

    "progression test: cannot resolve a higher-kinded type in a higher-kinded tag in an anonymous deeply-nested type lambda" in {
      val t = intercept[TestFailedException] {
        assertCompiles(
          """
      def mk[F[+_, +_]: TagKK] = TagKK[ ({ type l[E, A] = BIOService[ ({ type l[X, Y] = F[A, E] })#l ] })#l ]
      val tag = mk[Either]

      assert(tag.tag == LTagKK[ ({ type l[E, A] = BIOService[ ({ type l[X, Y] = Either[A, E] })#l ] })#l ].tag)
      """
        )
      }
      assert(
        t.getMessage.contains("could not find implicit value") ||
        t.getMessage.contains("diverging implicit") || /*2.11*/
        t.getMessage.contains("no implicit argument of type") || /*Dotty*/
        t.getMessage.contains("Cannot find implicit Tag") /*Dotty 3.1.3+*/
      )
    }

    "progression test: projections into singletons are not handled properly (on Scala 2)" in {
      trait A {
        class X

        final val singleton1 = "bar"
        type S1 = singleton1.type

        val singleton2 = "bar"
        type S2 = singleton2.type

//        val s1a = Tag[S1] // class type required but String("bar") found error on 2.11
        val s1a = LTT[S1]
        val s1a1 = Tag[singleton1.type].tag

//        val s2a = Tag[S2]
        val s2a = LTT[S2]
        val s2a1 = Tag[singleton2.type].tag
      }

      trait B extends A {
        val xb = Tag[X].tag

//        val s1b = Tag[S1].tag
        val s1b = LTT[S1]
        val s1b1 = Tag[singleton1.type].tag

        val s2b = LTT[S2]
        val s2b1 = Tag[singleton2.type].tag
      }

      object B extends B

      // Scala 2.12 doesn't handle literal types here
      if (LTT[B.singleton1.type] != LTT[String] && !IsDotty) {
        assertDifferent(Tag[A#S1].tag, LTT[String])
      }
      assertSame(Tag[A#S1].tag, B.s1a)
      assertSame(Tag[A#S1].tag, B.s1a1)
      assertSame(Tag[A#S1].tag, B.s1b)
      assertSame(Tag[A#S1].tag, B.s1b1)

      // progression: this still fails; see https://github.com/zio/izumi-reflect/issues/192
      //  projection into singleton generates a form `_1.singleton2.type forSome { val _1: A }` which is not handled on Scala 2
      doesntWorkYetOnScala2 {
        assertSame(Tag[A#S2].tag, B.s2a)
      }
      doesntWorkYetOnScala2 {
        assertSame(Tag[A#S2].tag, B.s2b)
      }
      doesntWorkYetOnScala2 {
        assertSame(Tag[A#S2].tag, B.s2a1)
      }
      doesntWorkYetOnScala2 {
        assertSame(Tag[A#S2].tag, B.s2b1)
      }
    }

    "Progression test: Scala 2 fails to Handle Tags outside of a predefined set (Somehow raw Tag.auto.T works on Scala 2, but not when defined as an alias)" in {
      type TagX[F[_, _, _[_[_], _], _[_], _]] = Tag.auto.T[F]
//      type TagX[K[_, _, _[_[_], _], _[_], _]] = HKTag[{ type Arg[T1, T2, T3[_[_], _], T4[_], T5] = K[T1, T2, T3, T4, T5] }]

      doesntWorkYetOnScala2 {
        assertCompiles(
          """
      def testTagX[F[_, _, _[_[_], _], _[_], _]: TagX, A: Tag, B: Tag, C[_[_], _]: TagTK, D[_]: TagK, E: Tag]: Tag[F[A, B, C, D, E]] = Tag[F[A, B, C, D, E]]
         """
        )
      }
      def testTagX[F[_, _, _[_[_], _], _[_], _]: Tag.auto.T, A: Tag, B: Tag, C[_[_], _]: TagTK, D[_]: TagK, E: Tag]: Tag[F[A, B, C, D, E]] = Tag[F[A, B, C, D, E]]

      val value = testTagX[TXU, Int, String, OptionT, List, Boolean]
      assert(value.tag == fromRuntime[TXU[Int, String, OptionT, List, Boolean]])
    }

    "progression test: fails to combine higher-kinded intersection types without losing ignored type arguments" in {
      def mk[F[+_, +_]: TagKK, G[+_, +_]: TagKK] = Tag[IntersectionBlockingIO[F, G]]
      val tag = mk[Either, IO]
      val tagMono = Tag[IntersectionBlockingIO[Either, IO]]

      doesntWorkYet {
        assertSameStrict(tag.tag, tagMono.tag)
      }
    }

    "progression test: Dotty fails to regression test: resolve correct closestClass for Scala vararg AnyVal (https://github.com/zio/izumi-reflect/issues/224)" in {
      val tag = Tag[VarArgsAnyVal]
      doesntWorkYetOnDotty {
        assert(tag.closestClass == classOf[scala.Seq[Any]])
      }
    }

    "progression test: fails to preserve lower bound when combining higher-kinded type members" in {
      def combine1[X[_[_], _]: TagTK, F[_]: TagK, A: Tag]: Tag[X[F, A]] = Tag[X[F, A]]
      def combine2[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

      val t1 = TagTK[HigherKindedTypeMember.T]
      val t2 = TagK[HigherKindedTypeMember.T[IO[Throwable, *], *]]

      val tres1 = combine1[HigherKindedTypeMember.T, IO[Throwable, *], Int](t1, implicitly, implicitly)
      val tres2 = combine2[HigherKindedTypeMember.T[IO[Throwable, *], *], Int](t2, implicitly)

      doesntWorkYet {
        assertChildStrict(Tag[Unit].tag, tres1.tag)
        assertChildStrict(Tag[Unit].tag, tres2.tag)
      }
    }

    // Can't fix this atm because simplification happens even without .simplified call because of https://github.com/lampepfl/dotty/issues/17544
    // The other way to fix this is to call `LightTypeTag.removeIntersectionTautologies` for every `Tag.refinedTag` -
    // but this would make such tags expensive to construct because the complexity is quadratic, with two <:< calls
    // per iteration.
    "progression test: fails on Scala 3 don't lose tautological intersection components other than Any/AnyRef" in {
      def tag1[T: Tag]: Tag[T with Trait1] = Tag[T with Trait1]
      def tag4[T: Tag]: Tag[T with Trait4] = Tag[T with Trait4]

      val t1 = tag1[Trait3[Dep]].tag
      val t2 = tag4[Trait3[Dep]].tag

      val t10 = Tag[Trait3[Dep] with Trait1].tag
      val t20 = Tag[Trait3[Dep] with Trait4].tag

      doesntWorkYetOnDotty {
        assertSameStrict(t1, t10)
        assertDebugSame(t1, t10)
      }

      assertSameStrict(t2, t20)
      assertDebugSame(t2, t20)
    }

    // We don't really want to fix it, because removing tautologies is quadratic, with two subtyping comparisions per step!
    // Would make construction really expensive, all for an extremely rare corner case
    "progression test: intersection tautologies are not removed automatically when constructing combined intersection type" in {
      def tag1[T: Tag]: Tag[T with Trait1] = Tag[T with Trait1]

      val t1 = tag1[Trait3[Dep]].tag

      val t10 = t1.removeIntersectionTautologies

      doesntWorkYetOnDotty {
        assertSameStrict(t1, t10)
        assertDebugSame(t1, t10)
      }
    }

  }

}
