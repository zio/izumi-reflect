package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.test.TestModel._
import izumi.reflect._
import izumi.reflect.test.PlatformSpecific.fromRuntime
import izumi.reflect.test.TestModel.x.SrcContextProcessor
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

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

    "progression test: cannot resolve a higher-kinded type in a higher-kinded tag in a named deeply-nested type lambda" in {
      val t = intercept[TestFailedException] {
        assertCompiles(
          """
      def mk[F[+_, +_]: TagKK] = TagKK[({ type l[A, B] = BIOServiceL[F, A, B] })#l]
      val tag = mk[Either]

      assert(tag.tag == LTagKK[Lambda[(E, A) => BIOService[Lambda[(X, Y) => Either[A, E]]]]].tag)
      """
        )
      }
      assert(
        t.message.get.contains("could not find implicit value") ||
        t.message.get.contains("diverging implicit") || /*2.11*/
        t.message.get.contains("no implicit argument of type") || /*Dotty*/
        t.message.get.contains("no given instance of type") /*Dotty 3.1.3+*/
      )
    }

    "progression test: cannot resolve a higher-kinded type in a higher-kinded tag in an anonymous deeply-nested type lambda" in {
      val t = intercept[TestFailedException] {
        assertCompiles(
          """
      def mk[F[+_, +_]: TagKK] = TagKK[ ({ type l[E, A] = BIOService[ ({ type l[X, Y] = F[A, E] })#l ] })#l ]
      val tag = mk[Either]

      assert(tag.tag == LTagKK[Lambda[(E, A) => BIOService[Lambda[(X, Y) => Either[A, E]]]]].tag)
      """
        )
      }
      assert(
        t.message.get.contains("could not find implicit value") ||
        t.message.get.contains("diverging implicit") || /*2.11*/
        t.message.get.contains("no implicit argument of type") || /*Dotty*/
        t.message.get.contains("no given instance of type") /*Dotty 3.1.3+*/
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

    "progression test: projections into singletons are not handled properly" in {
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
      //  projection into singleton generates a form `_1.singleton2.type forSome { val _1: A }` which is not handled
      doesntWorkYetOnDotty {
        assert(!Tag[A#S2].tag.debug().contains("::_$A::singleton2"))
      }
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

    "Work for structural concrete types doesn't work on Dotty" in {
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

  }

}
