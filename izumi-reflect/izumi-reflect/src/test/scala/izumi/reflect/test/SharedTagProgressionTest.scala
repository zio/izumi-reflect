package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.test.TestModel._
import izumi.reflect._
import izumi.reflect.test.PlatformSpecific.fromRuntime
import izumi.reflect.test.TestModel.x.SrcContextProcessor
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

    "progression test: Scala 2, reported in https://github.com/zio/izumi-reflect/issues/189, parameterized type alias with intersection produces incorrect output" in {
      def elementTag[F[_]: TagK]: Tag[SrcContextProcessor[F]] = Tag[TestModel.x.SrcContextProcessor[F]]
      assert(elementTag[CIO].tag == Tag[TestModel.x.SrcContextProcessor[CIO]].tag)

      doesntWorkYetOnScala2 {
        type K[F[_]] = Set[TestModel.x.SrcContextProcessor[F]]
        assert(TagT[K].tag.combine(TagK[CIO].tag) == Tag[Set[TestModel.x.SrcContextProcessor[CIO]]].tag)

        def aliasedTag[F[_]: TagK]: Tag[Set[SrcContextProcessor[F]]] = Tag[K[F]]
        assert(aliasedTag[CIO].tag == Tag[Set[TestModel.x.SrcContextProcessor[CIO]]].tag)

        def directTag[F[_]: TagK]: Tag[Set[SrcContextProcessor[F]]] = Tag[Set[TestModel.x.SrcContextProcessor[F]]]
        assert(directTag[CIO].tag == Tag[Set[TestModel.x.SrcContextProcessor[CIO]]].tag)
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
          t.get.message.get.contains("could not find implicit value") ||
          t.get.message.get.contains("diverging implicit") /*2.11*/
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
        t.message.get.contains("could not find implicit value") ||
        t.message.get.contains("diverging implicit") || /*2.11*/
        t.message.get.contains("no implicit argument of type") || /*Dotty*/
        t.message.get.contains("Cannot find implicit Tag") /*Dotty 3.1.3+*/
      )
    }

    "progression test: type tags with bounds are not currently requested by the macro" in {
      val t = intercept[TestFailedException] {
        assertCompiles("""
        type `TagK<:Dep`[K[_ <: Dep]] = HKTag[ { type Arg[A <: Dep] = K[A] } ]

        def t[T[_ <: Dep]: `TagK<:Dep`, A: Tag] = Tag[T[A]]

        assert(t[Trait3, Dep].tag == safe[Trait3[Dep]].tag)
        """)
      }
      assert(
        t.message.get.contains("could not find implicit value") ||
        t.message.get.contains("no implicit argument of type") /*Dotty*/
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

  }

}
