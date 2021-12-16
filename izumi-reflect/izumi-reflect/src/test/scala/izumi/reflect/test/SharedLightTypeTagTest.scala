package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.macrortti.LightTypeTagRef.{AppliedNamedReference, Boundaries}

import scala.annotation.StaticAnnotation
import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

abstract class SharedLightTypeTagTest extends TagAssertions {

  import TestModel._

  "lightweight type tags (all versions)" should {

    "support distinction between subtypes" in {
      val strLTT = LTT[String]
      val subStrALTT = LTT[SubStrA]
      val subStrCLTT = LTT[SubStrC]
      val subStrBLTT = LTT[SubStrB]
      val subStrDLTT = LTT[SubStrD]
      val subSubStrLTT = LTT[SubSubStr]
      val foo = LTT[SubStrA \/ Int]
      val bar = `LTT[_,_]`[\/].combine(subStrALTT, LTT[Int])
      val strUnpacker = LightTypeTagUnpacker(strLTT)
      val substrUnpacker = LightTypeTagUnpacker(subStrALTT)
      val subsubstrUnpacker = LightTypeTagUnpacker(subSubStrLTT)
      val strTR = strLTT.ref.asInstanceOf[AppliedNamedReference]
      val subStrTR = subStrALTT.ref.asInstanceOf[AppliedNamedReference]
      val subSubStrTR = subSubStrLTT.ref.asInstanceOf[AppliedNamedReference]

      assert(substrUnpacker.inheritance == strUnpacker.inheritance.map {
        case (s, v) if s.toString == "String" => subStrTR.asName.copy(boundaries = Boundaries.Empty) -> (v + strTR.asName)
        case p => p
      })
      assert(subsubstrUnpacker.inheritance == strUnpacker.inheritance.map {
        case (s, v) if s.toString == "String" => subSubStrTR.asName.copy(boundaries = Boundaries.Empty) -> (v + strTR.asName)
        case p => p
      })

      assertDifferent(subStrALTT, strLTT)
      assertChild(subStrALTT, strLTT)
      assertChild(subSubStrLTT, strLTT)
      assertChild(subSubStrLTT, subStrALTT)
      assertNotChild(strLTT, subStrALTT)
      assertNotChild(subStrALTT, subSubStrLTT)
      assertNotChild(subSubStrLTT, subStrBLTT)
      assertDifferent(subStrALTT, subStrBLTT)

      assertSame(subStrCLTT, strLTT)
      assertChild(subStrCLTT, strLTT)
      assertChild(strLTT, subStrCLTT)

      assertNotChild(subStrALTT, subStrBLTT)
      assertSame(subStrALTT, subStrDLTT)
      assertSame(foo, bar)

      // see https://github.com/7mind/izumi/pull/1528
      assertSame(strLTT.combine(), strLTT)
    }

    "eradicate tautologies with Any/Object" in {
      assertSame(LTT[Object with Option[String]], LTT[Option[String]])
      assertSame(LTT[Any with Option[String]], LTT[Option[String]])
      assertSame(LTT[AnyRef with Option[String]], LTT[Option[String]])

      assertSameRef(LTT[Object with Option[String]], LTT[Option[String]])
      assertSameRef(LTT[Any with Option[String]], LTT[Option[String]])
      assertSameRef(LTT[AnyRef with Option[String]], LTT[Option[String]])

      assertChild(LTT[Object with Option[String]], LTT[Option[String]])
      assertChild(LTT[Any with Option[String]], LTT[Option[String]])
      assertChild(LTT[AnyRef with Option[String]], LTT[Option[String]])

      assertChild(LTT[Option[String]], LTT[Object with Option[String]])
      assertChild(LTT[Option[String]], LTT[Any with Option[String]])
      assertChild(LTT[Option[String]], LTT[AnyRef with Option[String]])
    }

    "support self-intersection (X with X)" in {
      assertSame(`LTT`[String with String], `LTT`[String])
    }

    "support subtype checks" in {
      assertChild(LTT[Int], LTT[AnyVal])
      assertChild(LTT[Int], LTT[Int])
      assertChild(LTT[List[Int]], LTT[List[Int]])
      assertChild(LTT[List[I2]], LTT[List[I1]])
      assertChild(LTT[Either[Nothing, Int]], LTT[Either[Throwable, Int]])

      assertChild(LTT[F2[I2]], LTT[F1[I1]])
      assertChild(LTT[FT2[IT2]], LTT[FT1[IT2]])

      assertChild(LTT[List[Int]], `LTT[_]`[List])
      assertNotChild(LTT[Set[Int]], `LTT[_]`[Set])

      assertChild(LTT[FM2[I2]], LTT[FM1[I1, Unit]])
      assertChild(LTT[FM2[I2]], `LTT[_,_]`[FM1])
      assertChild(LTT[Option[Nothing]], LTT[Option[Int]])

      assertChild(`LTT[_[_],_[_]]`[P1].combine(`LTT[_]`[X1], `LTT[_]`[X2]), LTT[P0[X2, X1]])
      assertNotChild(`LTT[_[_],_[_]]`[P1].combine(`LTT[_]`[X1], `LTT[_]`[X2]), LTT[P0[X1, X2]])

      assertChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X1]), LTT[P0[X2, X1]])
      assertChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X2, X2]])
      assertNotChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X2, X1]])
      assertNotChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X1, X2]])
    }

    "support swapped parents" in {
      trait KT1[+A1, +B1]
      trait KT2[+A2, +B2] extends KT1[B2, A2]

      assertChild(LTT[KT2[H1, I1]], LTT[KT1[I1, H1]])
      assertNotChild(LTT[KT2[H1, I1]], LTT[KT1[H1, I1]])

      assertChild(LTT[KT2[H2, I2]], LTT[KT1[I1, H1]])
      assertNotChild(LTT[KT2[H2, I2]], LTT[KT1[H1, I1]])

      trait KK1[+A, +B, +U]
      trait KK2[+A, +B] extends KK1[B, A, Unit]

      assertChild(LTT[KK2[Int, String]], LTT[KK1[String, Int, Unit]])

      assertChild(LTT[KK2[H2, I2]], LTT[KK1[I1, H1, Unit]])
      assertNotChild(LTT[KK2[H2, I2]], LTT[KK1[H1, I1, Unit]])

      assertNotChild(`LTT[_]`[KK2[H2, *]], `LTT[_]`[KK1[H1, *, Unit]])
    }

    "support subtyping of parents parameterized with type lambdas" in {
      assertChild(LTT[RoleChild[Either]], LTT[RoleParent[Either[Throwable, *]]])
    }

    "support subtyping of parents parameterized with type lambdas in combined tags" in {
      val childBase = `LTT[_[_,_]]`[RoleChild]
      val childArg = `LTT[_,_]`[Either]
      val combinedTag = childBase.combine(childArg)

      assertSame(combinedTag, LTT[RoleChild[Either]])
    }

    "support subtyping of parents parameterized with type lambdas in combined tags with multiple parameters" in {
      val childBase = `LTT[_[+_,+_],_,_]`[RoleChild2]
      val childArgs = Seq(`LTT[_,_]`[Either], LTT[Int], LTT[String])
      val combinedTag = childBase.combine(childArgs: _*)
      val expectedTag = LTT[RoleParent[Either[Throwable, *]]]
      val noncombinedTag = LTT[RoleChild2[Either, Int, String]]

      assertSame(combinedTag, noncombinedTag)
      assertChild(noncombinedTag, expectedTag)
    }

    "support PDTs" in {
      val a = new C {
        override type A = Int
      }
      val a0 = new C {}
      val a1: C = new C {
        override type A = Int
      }
      val a2 = new C {
        override type A = String
      }

      assertSame(LTT[a.A], LTT[Int])
      assertDifferent(LTT[a0.A], LTT[Int])
      assertDifferent(LTT[a1.A], LTT[Int])
      assertDifferent(LTT[a1.A], LTT[a2.A])
      assertChild(LTT[a1.A], LTT[a1.A])
      assertSame(LTT[a2.A], LTT[String])
    }

    "intersections are associative" in {
      type F1 = (W3[Int] with W1) with I1
      type F2 = W3[Int] with (W1 with I1)

      type T1[A] = (W3[A] with W1) with I1
      type T2[A] = W3[A] with (W1 with I1)

      assertDeepSame(LTT[F1], LTT[F2])
      assertDeepSame(`LTT[_]`[T1], `LTT[_]`[T2])
    }

    "runtime-combined intersections are associative" in {
      type F1 = W3[Int] with W1
      type F11 = (W3[Int] with W1) with I1
      type F12 = W3[Int] with (W1 with I1)

      type T1[A] = W3[Int] with (W1 with A)
      type T2[A] = (W3[Int] with W1) with A

      assertIntersection(List(LTT[F1], LTT[I1]), LTT[F11])
      assertIntersection(List(LTT[F1], LTT[I1]), LTT[F12])

      assertCombine(`LTT[_]`[T1], LTT[I1], LTT[F11])
      assertCombine(`LTT[_]`[T1], LTT[I1], LTT[F12])

      assertCombine(`LTT[_]`[T2], LTT[I1], LTT[F11])
      assertCombine(`LTT[_]`[T2], LTT[I1], LTT[F12])
    }

    "support type alias and refinement subtype checks" in {
      assertChild(LTT[XS], LTT[WithX])
      assertChild(LTT[XS], LTT[{ type X }])
      assertNotChild(LTT[WithX], LTT[XS])
      assertNotChild(LTT[{ type X }], LTT[XS])
    }

    "support literal types" in {
      assertSame(literalLtt("str2"), literalLtt("str2"))
      assertDifferent(literalLtt("str1"), literalLtt("str2"))

      assertChild(literalLtt("str"), LTT[String])
      assertNotChild(literalLtt("str"), LTT[Int])
      assertNotChild(LTT[String], literalLtt("str"))
      assertDifferent(LTT[String], literalLtt("str"))
    }

    "resolve comparisons of object and trait with the same name" in {
      assertNotChild(LTT[YieldOpCounts.type], LTT[RoleChild[Either]])
      assertChild(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertDifferent(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertNotChild(LTT[YieldOpCounts], LTT[YieldOpCounts.type])
    }

    "resolve prefixes of annotated types" in {
      assert(LTT[TPrefix.T @unchecked] == LTT[TPrefix.T])
    }

    "`withoutArgs` comparison works" in {
      assert(LTT[Set[Int]].ref.withoutArgs == LTT[Set[Any]].ref.withoutArgs)
      assert(LTT[Either[Int, String]].ref.withoutArgs == LTT[Either[Boolean, List[Int]]].ref.withoutArgs)

      assertSame(LTT[Set[Int]].withoutArgs, LTT[Set[Any]].withoutArgs)

      assertChild(LTT[mutable.LinkedHashSet[Int]].withoutArgs, LTT[collection.Set[Any]].withoutArgs)
      assertChild(LTT[ListSet[Int]].withoutArgs, LTT[collection.Set[Any]].withoutArgs)
      assertChild(LTT[ListSet[Int]].withoutArgs, LTT[immutable.Set[Any]].withoutArgs)
      assertChild(LTT[BitSet].withoutArgs, LTT[collection.Set[Any]].withoutArgs)
    }

    "`typeArgs` works" in {
      assertSame(LTT[Set[Int]].typeArgs.head, LTT[collection.Set[Int]].typeArgs.head)
      assertChild(LTT[Set[Int]].typeArgs.head, LTT[collection.Set[AnyVal]].typeArgs.head)

      assert(`LTT[_,_]`[Either].typeArgs.isEmpty)
      assert(`LTT[_]`[Either[String, *]].typeArgs == List(LTT[String]))
    }

    "support subtyping of a simple combined type" in {
      val ctor = `LTT[_[_]]`[ApplePaymentProvider]
      val arg = `LTT[_]`[Id]
      val combined = ctor.combine(arg)
      assertChild(combined, LTT[H1])
    }

    "issue #762: properly strip away annotated types / empty refinements / type aliases" in {
      val predefString = LTT[String]
      val javaLangString = LTT[java.lang.String]
      val weirdPredefString = LTT[(scala.Predef.String {}) @IdAnnotation("abc")]

      assertSame(predefString, javaLangString)
      assertSame(predefString, weirdPredefString)
      assertSame(javaLangString, weirdPredefString)

      assertDebugSame(predefString, javaLangString)
      assertDebugSame(predefString, weirdPredefString)
      assertDebugSame(javaLangString, weirdPredefString)

      val javaLangOpt = LTT[Option[java.lang.String]]
      val predefOpt = LTT[Option[scala.Predef.String]]
      val weirdPredefOpt = LTT[Option[(scala.Predef.String {}) @IdAnnotation("x")]]

      assertDebugSame(javaLangOpt, predefOpt)
      assertDebugSame(javaLangOpt, weirdPredefOpt)
    }

    "calculate identical hashCode in parsed and constructed instances" in {
      val tag1 = LTT[String]
      val tag2 = tag1.withoutArgs
      assertSameRef(tag1, tag2)
      assertSame(tag1, tag2)
      assert(tag1.hashCode() == tag2.hashCode())
      assert(tag1.hashCode() != 0)
      assert(tag1.ref.hashCode() != 0)
    }

    "support non-positional typetag combination" in {
      assertCombineNonPos(`LTT[_,_]`[Either], Seq(None, Some(LTT[Unit])), `LTT[_]`[Either[*, Unit]])
    }

    "support additional mixin traits after first trait with a HKT parameter" in {
      assertChild(LTT[J[Option]], LTT[J1[Option]])
      assertChild(LTT[J[Option]], LTT[J3])
      assertChild(LTT[J[Option]], LTT[J2])
      assertChild(LTT[J[Option]], LTT[J1[Option] with J2])
      assertChild(LTT[J[Option]], LTT[J2 with J3])
      assertChild(LTT[J[Option]], LTT[J1[Option] with J2 with J3])
    }

    "support LTagK* family summoners" in {
      val tag = LTagK[List].tag
      val tag1 = LTagKK[Either].tag
      assertSame(tag, `LTT[_]`[List])
      assertSame(tag1, `LTT[_,_]`[Either])
    }

    "support higher-kinded intersection type equality" in {
      type T1[A] = W3[A] with W1
      type T2[A] = W4[A] with W2

      assertSame(`LTT[_]`[T1], `LTT[_]`[T1])
      assertDifferent(`LTT[_]`[T1], `LTT[_]`[T2])
    }

    "support contravariance" in {
      assertDeepChild(LTT[Any => Int], LTT[Int => Int])
    }

    "support typetag combination" in {
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[Id], LTT[T1[Id]])
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[FP], LTT[T1[FP]])
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[FI], LTT[T1[FI]])

      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[List], LTT[T1[List]])
      assertCombine(`LTT[_]`[List], LTT[Int], LTT[List[Int]])
      assertCombine(`LTT[_,_]`[Either], LTT[Unit], `LTT[_]`[Either[Unit, *]])

      assertCombine(`LTT[_[_[_],_[_]]]`[T2], `LTT[_[_],_[_]]`[T0], LTT[T2[T0]])

      type ComplexRef[T] = W1 with T { def a(p: T): T; type M = T }
      assertCombine(`LTT[_]`[ComplexRef], LTT[Int], LTT[W1 with Int { def a(p: Int): Int; type M = Int }])
    }

    "tautological intersections with Any/Object are discarded from internal structure" in {
      assertDeepSame(LTT[(Object {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDeepSame(LTT[(Any {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDeepSame(LTT[(AnyRef {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])

      assertDebugSame(LTT[(Object {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDebugSame(LTT[(Any {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDebugSame(LTT[(AnyRef {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
    }

  }

}
