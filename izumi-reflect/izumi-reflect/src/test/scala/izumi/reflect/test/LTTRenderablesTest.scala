package izumi.reflect.test

import izumi.reflect.macrortti._

class LTTRenderablesTest extends TagAssertions {

  object X {
    object Y {
      class C
    }
  }

  "LTT renderables" should {
    "render simple lambdas using placeholders when using scalaStyledRepr" in {
      val list = `LTT[_]`[List].scalaStyledRepr
      val either = `LTT[_,_]`[Either].scalaStyledRepr
      val either2 = `LTT[_]`[Either[Int, *]].scalaStyledRepr
      val either3 = `LTT[_]`[Either[*, Int]].scalaStyledRepr
      val optionT = `LTT[_]`[OptionT[List, *]].scalaStyledRepr

      assert(list == "scala.collection.immutable.List[+_]")
      assert(either == "scala.util.Either[+_,+_]")
      assert(either2 == "scala.util.Either[+scala.Int,+_]")
      assert(either3 == "scala.util.Either[+_,+scala.Int]")
      assert(optionT == "izumi.reflect.test.OptionT[=scala.collection.immutable.List[+_],=_]")
    }

    "render complex lambdas using long form when using scalaStyledRepr" in {
      type Const[+A, +B] = B
      type SwapEither[+A, +B] = Either[B, A]
      type SwapOptionT[A, B[_]] = OptionT[B, A]
      type UseInner[A] = OptionT[Either[A, *], A]
      type UseInner2[A, B] = OptionT[Either[A, *], B]
      type Reuse[A] = Either[A, A]

      val identity = `LTT[_]`[ID.Identity].scalaStyledRepr
      val const = `LTT[_,_]`[Const].scalaStyledRepr
      val swapEither = `LTT[_,_]`[SwapEither].scalaStyledRepr
      val swapOptionT = `LTT[_]`[SwapOptionT[*, ID.Identity]].scalaStyledRepr
      val useInner = `LTT[_]`[UseInner].scalaStyledRepr
      val useInner2 = `LTT[_,_]`[UseInner2].scalaStyledRepr
      val reuse = `LTT[_]`[Reuse].scalaStyledRepr

      assert(identity == "[A] ➾ A")
      assert(const == "[A,B] ➾ B")
      assert(swapEither == "[A,B] ➾ scala.util.Either[+B,+A]")
      val expectedDepth = if (IsScala3) 1 else 2
      assert(swapOptionT == s"izumi.reflect.test.OptionT[=[A$expectedDepth] ➾ A$expectedDepth,=_]")
      assert(useInner == s"[A] ➾ izumi.reflect.test.OptionT[[A$expectedDepth] ➾ scala.util.Either[+A,+A$expectedDepth],A]")
      assert(useInner2 == s"[A,B] ➾ izumi.reflect.test.OptionT[[A$expectedDepth] ➾ scala.util.Either[+A,+A$expectedDepth],B]")
      assert(reuse == "[A] ➾ scala.util.Either[+A,+A]")
    }

    "types in nested objects should be rendered with dot separator" in {
      assert(LTT[X.Y.C].scalaStyledRepr == "izumi.reflect.test.LTTRenderablesTest.X.Y.C")
    }
  }

}
