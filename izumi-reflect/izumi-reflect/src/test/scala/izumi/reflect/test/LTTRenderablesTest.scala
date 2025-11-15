package izumi.reflect.test

import izumi.reflect.macrortti._

class LTTRenderablesTest extends TagAssertions {

  "LTT renderables" should {
    "render simple lambdas using placeholders when using scalaStyledName" in {
      val list = `LTT[_]`[List].scalaStyledName
      val either = `LTT[_,_]`[Either].scalaStyledName
      val either2 = `LTT[_]`[Either[Int, *]].scalaStyledName
      val either3 = `LTT[_]`[Either[*, Int]].scalaStyledName
      val optionT = `LTT[_]`[OptionT[List, *]].scalaStyledName

      assert(list == "scala.collection.immutable.List[+_]")
      assert(either == "scala.util.Either[+_,+_]")
      assert(either2 == "scala.util.Either[+scala.Int,+_]")
      assert(either3 == "scala.util.Either[+_,+scala.Int]")
      assert(optionT == "izumi.reflect.test.OptionT[=scala.collection.immutable.List[+_],=_]")
    }

    "render complex lambdas using long form when using scalaStyledName" in {
      type Const[+A, +B] = B
      type SwapEither[+A, +B] = Either[B, A]
      type SwapOptionT[A, B[_]] = OptionT[B, A]
      type UseInner[A] = OptionT[Either[A, *], A]
      type UseInner2[A, B] = OptionT[Either[A, *], B]
      type Reuse[A] = Either[A, A]

      val identity = `LTT[_]`[ID.Identity].scalaStyledName
      val const = `LTT[_,_]`[Const].scalaStyledName
      val swapEither = `LTT[_,_]`[SwapEither].scalaStyledName
      val swapOptionT = `LTT[_]`[SwapOptionT[*, ID.Identity]].scalaStyledName
      val useInner = `LTT[_]`[UseInner].scalaStyledName
      val useInner2 = `LTT[_,_]`[UseInner2].scalaStyledName
      val reuse = `LTT[_]`[Reuse].scalaStyledName

      assert(identity == "[A] ➾ A")
      assert(const == "[A,B] ➾ B")
      assert(swapEither == "[A,B] ➾ scala.util.Either[+B,+A]")
      val expectedDepth = if (IsScala3) 1 else 2
      assert(swapOptionT == s"izumi.reflect.test.OptionT[=[A$expectedDepth] ➾ A$expectedDepth,=_]")
      assert(useInner == s"[A] ➾ izumi.reflect.test.OptionT[[A$expectedDepth] ➾ scala.util.Either[+A,+A$expectedDepth],A]")
      assert(useInner2 == s"[A,B] ➾ izumi.reflect.test.OptionT[[A$expectedDepth] ➾ scala.util.Either[+A,+A$expectedDepth],B]")
      assert(reuse == "[A] ➾ scala.util.Either[+A,+A]")
    }
  }

}
