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

    "progression test: can't handle parameters in defs/vals in structural types" in {
      def t1[T: Tag]: Tag[{ def x: T }] = Tag[{ def x: T }]
      def t2[T: Tag]: Tag[{ val x: T }] = Tag[{ val x: T }]

      doesntWorkYetOnScala2 {
        assert(t1[Int].tag == Tag[{ def x: Int }].tag)
      }
      doesntWorkYetOnScala2 {
        assert(t2[Int].tag == Tag[{ val x: Int }].tag)
      }
    }

    "progression test: reported in https://github.com/zio/izumi-reflect/issues/189, parameterized type alias with intersection produces incorrect output" in {
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

    "progression test: Work for structural concrete types doesn't work on Dotty" in {
      assert(Tag[{ def a: Int; def g: Boolean }].tag == fromRuntime[{ def a: Int; def g: Boolean }])
      assert(Tag[Int { def a: Int }].tag == fromRuntime[Int { def a: Int }])

      assert(Tag[With[str.type] with ({ type T = str.type with Int })].tag == fromRuntime[With[str.type] with ({ type T = str.type with Int })])
      doesntWorkYetOnDotty {
        assert(Tag[With[str.type] with ({ type T = str.type with Int })].tag != fromRuntime[With[str.type] with ({ type T = str.type with Long })])
      }
    }

    "progression test: subtyping for Invariant Java HKT doesn't work on Dotty" in {
      val collection = TagK[java.util.Collection]
      val javaIterable = TagK[java.lang.Iterable]
      doesntWorkYetOnDotty {
        assertChildStrict(collection.tag, javaIterable.tag)
      }
    }

    "progression test: subtyping for Invariant Scala HKT doesn't work on Dotty" in {
      val mutableSet = TagK[scala.collection.mutable.Set]
      val collectionSet = TagK[scala.collection.Set]
      doesntWorkYetOnDotty {
        assertChildStrict(mutableSet.tag, collectionSet.tag)
      }
    }

    "progression test: Dotty fails regression test: https://github.com/zio/izumi-reflect/issues/82, convert trifunctor hkt to bifunctor when combining tags" in {
      import TestModel._
      def tag[F[-_, +_, +_]: TagK3] = Tag[BIO2[F[Any, +*, +*]]]
      doesntWorkYetOnDotty {
        assertChild(tag[ZIO].tag, Tag[BIO2[IO]].tag)
      }
      doesntWorkYetOnDotty {
        assertChild(Tag[BIO2[IO]].tag, tag[ZIO].tag)
      }
      doesntWorkYetOnDotty {
        assertSame(tag[ZIO].tag, Tag[BIO2[IO]].tag)
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

    "progression test: Dotty fails to combine higher-kinded types without losing ignored type arguments" in {
      def mk[F[+_, +_]: TagKK] = Tag[BlockingIO[F]]
      val tag = mk[IO]
      val tagMono = Tag[BlockingIO[IO]]

      doesntWorkYetOnDotty {
        assertSameStrict(tag.tag, tagMono.tag)
      }
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
      doesntWorkYetOnDotty {
        assert(Tag[VarArgsAnyVal].closestClass == classOf[scala.Seq[Any]])
      }
    }

    "progression test: Dotty fails to can resolve parameters in structural types" in {
      def t[X: Tag]: Tag[{ type T = X }] = Tag[{ type T = X }]

      assertSame(t[Int].tag, Tag[{ type T = Int }].tag)
      doesntWorkYetOnDotty {
        assertDifferent(t[Int].tag, Tag[{ type T = String }].tag)
      }
    }

    "progression test: Dotty fails to resolve a higher-kinded type inside an anonymous type lambda with ignored & higher-kinded type arguments" in {
      def mk[F[_[_], _]: TagTK] = Tag[BlockingIO3T[({ type l[R, E[_], A] = F[E, A] })#l]]
      val tag = mk[OptionT]

      doesntWorkYetOnDotty {
        assert(tag.tag == Tag[BlockingIOT[OptionT]].tag)
      }
    }

    "progression test: Dotty fails to correctly resolve a higher-kinded nested type inside an anonymous swap type lambda" in {
      def mk[F[+_, +_]: TagKK] = Tag[BIOService[λ[(E, A) => F[A, E]]]]
      val tag = mk[Either]

      doesntWorkYetOnDotty {
        assert(tag.tag == Tag[BIOService[SwapF2[Either, *, *]]].tag)
      }
      doesntWorkYetOnDotty {
        assert(tag.tag == Tag[BIOService[Swap]].tag)
      }
      doesntWorkYetOnDotty {
        assert(tag.tag == Tag[BIOService[λ[(E, A) => Either[A, E]]]].tag)
      }
    }

    "progression test: Dotty fails to Handle abstract types instead of parameters" in {
      trait T1 {
        type F[F0[_], A0] = OptionT[F0, A0]
        type C[_, _]
        type G[_]
        type A
        type B

        def x: Tag[F[G, Either[A, B]]]
      }

      val t1: T1 {
        type G[T] = List[T]
        type C[A0, B0] = Either[A0, B0]
        type A = Int
        type B = Byte
      } = new T1 {
        type G[T] = List[T]
        type C[A0, B0] = Either[A0, B0]
        type A = Int
        type B = Byte

        // Inconsistent handling of type aliases by scalac...
        // No TagK for G, but if G is inside an object or enclosing class
        // then there is a TagK
        val g: TagK[G] = TagK[List]

        final val x: Tag[F[G, Either[A, B]]] = {
          implicit val g0: TagK[G] = g
          val _ = g0
          Tag[F[G, C[A, B]]]
        }
      }

//      withDebugOutput {
      doesntWorkYetOnDotty {
        assertSameStrict(t1.x.tag, fromRuntime[OptionT[List, Either[Int, Byte]]])
      }
//      }
    }

    "Work for any abstract type with available Tag while preserving additional type refinement" in {
      def testTag[T: Tag] = Tag[T { type X = Int; type Y = String }]

      assertSameStrict(testTag[String].tag, fromRuntime[String { type X = Int; type Y = String }])
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { type X = String; type Y = Boolean }])
      }
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { type X = String; type Y = Boolean }])
      }
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { type X = Int; type Y = Boolean }])
      }
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { type X = Boolean; type Y = String }])
      }
    }

    "Work for any abstract type with available Tag while preserving additional method refinement" in {
      def testTag[T: Tag] = Tag[T { def x: Int; val y: String }]

      assertSameStrict(testTag[String].tag, fromRuntime[String { def x: Int; val y: String }])
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { def x: String; val y: Boolean }])
      }
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { def x: Int; val y: Boolean }])
      }
      doesntWorkYetOnDotty {
        assertNotChildStrict(testTag[String].tag, fromRuntime[String { def x: Boolean; val y: String }])
      }
    }

  }

}
