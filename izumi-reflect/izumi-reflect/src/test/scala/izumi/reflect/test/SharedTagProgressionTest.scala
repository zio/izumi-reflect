package izumi.reflect.test

import izumi.reflect.macrortti.LTT
import izumi.reflect.test.TestModel._
import izumi.reflect._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

abstract class SharedTagProgressionTest extends AnyWordSpec with TagAssertions with TagProgressions with InheritedModelKindProjector {

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
      assert(t.message.get.contains("could not find implicit value") || t.message.get.contains("diverging implicit") /*2.11*/ )
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
      assert(t.message.get.contains("could not find implicit value") || t.message.get.contains("diverging implicit") /*2.11*/ )
    }

    "progression test: type tags with bounds are not currently requested by the macro" in {
      val t = intercept[TestFailedException] {
        assertCompiles("""
        type `TagK<:Dep`[K[_ <: Dep]] = HKTag[ { type Arg[A <: Dep] = K[A] } ]

        def t[T[_ <: Dep]: `TagK<:Dep`, A: Tag] = Tag[T[A]]

        assert(t[Trait3, Dep].tag == safe[Trait3[Dep]].tag)
        """)
      }
      assert(t.message.get contains "could not find implicit value")
    }

    "progression test: projections into singletons are not handled properly" in {
      trait A {
        val singleton2 = "bar"
        type S2 = singleton2.type

        val s2a = LTT[S2]
        val s2a1 = Tag[singleton2.type].tag
      }

      trait B extends A {
        val s2b = LTT[S2]
        val s2b1 = Tag[singleton2.type].tag
      }

      object B extends B

      // progression: this still fails; see https://github.com/zio/izumi-reflect/issues/192
      //  projection into singleton generates a form `_1.singleton2.type forSome { val _1: A }` which is not handled
      doesntWorkYet {
        assert(Tag[A#S2].tag == B.s2a)
      }
      doesntWorkYet {
        assert(Tag[A#S2].tag == B.s2b)
      }
      doesntWorkYet {
        assert(Tag[A#S2].tag == B.s2a1)
      }
      doesntWorkYet {
        assert(Tag[A#S2].tag == B.s2b1)
      }
    }

  }

}
