package izumi.reflect.test

import izumi.reflect.macrortti.LightTypeTagRef.{AppliedNamedReference, Boundaries}
import izumi.reflect.macrortti.{LTT, _}

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

      assertChildStrict(subStrALTT, strLTT)
      assertChildStrict(subSubStrLTT, strLTT)
      assertChildStrict(subSubStrLTT, subStrALTT)
      assertNotChildStrict(subSubStrLTT, subStrBLTT)

      assertSameStrict(subStrCLTT, strLTT)

      assertNotChild(subStrALTT, subStrBLTT)
      assertSame(subStrALTT, subStrDLTT)
      assertSame(foo, bar)

      // see https://github.com/7mind/izumi/pull/1528
      assertSame(strLTT.combine(), strLTT)
    }

    "eradicate tautologies with Any/Object" in {
      assertSameStrict(LTT[Object with Option[String]], LTT[Option[String]])
      assertSameStrict(LTT[Any with Option[String]], LTT[Option[String]])
      assertSameStrict(LTT[AnyRef with Option[String]], LTT[Option[String]])
    }

    "eradicate self-intersection (X with X)" in {
      assertSameStrict(`LTT`[String with String], `LTT`[String])
    }

    "support subtype checks" in {
      assertChild(LTT[Int], LTT[AnyVal])
      assertChild(LTT[Int], LTT[Int])
      assertChild(LTT[List[Int]], LTT[List[Int]])
      assertChild(LTT[List[I2]], LTT[List[I1]])
      assertChild(LTT[Either[Nothing, Int]], LTT[Either[Throwable, Int]])

      assertChild(LTT[F2[I2]], LTT[F1[I1]])
      assertChild(LTT[FT2[IT2]], LTT[FT1[IT2]])

      assertChild(LTT[FM2[I2]], LTT[FM1[I1, Unit]])

      assertChild(LTT[Option[Nothing]], LTT[Option[Int]])

      assertChild(`LTT[_[_],_[_]]`[P1].combine(`LTT[_]`[X1], `LTT[_]`[X2]), LTT[P0[X2, X1]])
      assertNotChild(`LTT[_[_],_[_]]`[P1].combine(`LTT[_]`[X1], `LTT[_]`[X2]), LTT[P0[X1, X2]])

      assertChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X1]), LTT[P0[X2, X1]])
      assertChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X2, X2]])
      assertNotChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X2, X1]])
      assertNotChild(`LTT[_[_]]`[XP1].combine(`LTT[_]`[X2]), LTT[P0[X1, X2]])
    }

    "support unsound subtype checks" in {
      // UNSOUND-LAMBDA-COMPARISON
      // it's hard to say how should we compare propers with lambdas...

      // these two may be inverted by uncommenting corresponding lines in inheritance checks
      assertNotChild(LTT[FM2[I2]], `LTT[_,_]`[FM1])
      assertNotChild(LTT[List[Int]], `LTT[_]`[List])

      // this one should be always false
      assertNotChild(LTT[Set[Int]], `LTT[_]`[Set])
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

      assertSameStrict(LTT[F1], LTT[F2])
      assertSameStrict(`LTT[_]`[T1], `LTT[_]`[T2])
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
      assertChildStrict(LTT[XS], LTT[WithX])
      assertChildStrict(LTT[XS], LTT[{ type X }])
    }

    "support literal types" in {
      assertSame(literalLtt("str2"), literalLtt("str2"))
      assertDifferent(literalLtt("str1"), literalLtt("str2"))

      assertChildStrict(literalLtt("str"), LTT[String])
      assertNotChild(literalLtt("str"), LTT[Int])
    }

    "resolve comparisons of object and trait with the same name" in {
      assertNotChild(LTT[YieldOpCounts.type], LTT[RoleChild[Either]])
      assertChild(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertDifferent(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertNotChild(LTT[YieldOpCounts], LTT[YieldOpCounts.type])
    }

    "resolve prefixes of annotated types" in {
      assertSame(LTT[TPrefix.T @unchecked], LTT[TPrefix.T])
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

      val tag = `LTT[_[_]]`[({ type l[F[_]] = T0[List, F] })#l]
      val tagApplied = tag.combine(`LTT[_]`[Option])
      assertSame(tagApplied, LTT[T0[List, Option]])
      assert(tag.typeArgs == List(`LTT[_]`[List]))
      assert(tagApplied.typeArgs == List(`LTT[_]`[List], `LTT[_]`[Option]))
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
      assertChildStrict(LTT[Any => Int], LTT[Int => Int])
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

      assertCombine(`LTT[_[_]]`[({ type l[K[_]] = T0[Id, K] })#l], `LTT[_]`[FP], LTT[T0[Id, FP]])
    }

    "tautological intersections with Any/Object are discarded from internal structure" in {
      assertSameStrict(LTT[(Object {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertSameStrict(LTT[(Any {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertSameStrict(LTT[(AnyRef {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])

      assertDebugSame(LTT[(Object {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDebugSame(LTT[(Any {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
      assertDebugSame(LTT[(AnyRef {}) @IdAnnotation("x") with Option[(String with Object) {}]], LTT[Option[String]])
    }

    "wildcards are supported" in {
      assertDifferent(LTT[Set[_]], LTT[Set[Any]])
      assertDifferent(LTT[List[_]], LTT[List[Any]])
      assertChild(LTT[Set[Int]], LTT[Set[_]])
      assertNotChild(LTT[Set[_]], LTT[Set[Int]])
      assertChild(LTT[List[Int]], LTT[List[_]])
      assertNotChild(LTT[List[_]], LTT[List[Int]])
      assertChild(LTT[Int => Int], LTT[_ => Int])
    }

    "wildcards with bounds are supported" in {
      assertDifferent(LTT[Option[W1]], LTT[Option[_ <: W1]])
      assertDifferent(LTT[Option[H2]], LTT[Option[_ >: H4 <: H2]])
      assertDifferent(LTT[Option[Any]], LTT[Option[_ >: H4]])
    }

    "generate tags for wildcards with type boundaries" in {
      assertDifferent(LTT[Option[W1]], LTT[Option[_ <: W1]])
      assertChild(LTT[Option[W1]], LTT[Option[_ <: W1]])
      assertChild(LTT[Option[W2]], LTT[Option[_ <: W1]])
      assertNotChild(LTT[Option[W2]], LTT[Option[_ <: I1]])

      assertChild(LTT[Option[_ <: W2]], LTT[Option[W1]])
      assertChild(LTT[Option[_ <: W2]], LTT[Option[W2]])
      assertNotChild(LTT[Option[_ <: I1]], LTT[Option[W2]])

      assertChild(LTT[Option[H3]], LTT[Option[_ >: H4 <: H2]])
      assertNotChild(LTT[Option[H1]], LTT[Option[_ >: H4 <: H2]])

      assertTypeError("val o: Option[H3] = None: Option[_ >: H4 <: H2]")
      assertNotChild(LTT[Option[_ >: H4 <: H2]], LTT[Option[H3]])
      assertChild(LTT[Option[_ >: H4 <: H2]], LTT[Option[H1]])

      if (!IsDotty) {
        assertCompiles("val opt: Option[_ >: H4 <: H2] = None: Option[H5]")
      } else {
        assertCompiles("val opt: Option[_ >: H4 <: H2] = (None: Option[H5]): Option[H4]")
      }
      assertChild(LTT[Option[H4]], LTT[Option[_ >: H4 <: H2]])
      assertChild(LTT[Option[H2]], LTT[Option[_ >: H4 <: H2]])
      // this is counterintuitive but that's how Scala works, it ignores lower boundary in contravariant position
      assertChild(LTT[Option[H5]], LTT[Option[_ >: H4 <: H2]])
    }

    "https://github.com/zio/izumi-reflect/issues/315 regression test 2.1.0-M1: IntegrationCheck[F] should not be related to IntegrationCheck[Identity]" in {
      assertNotChildStrict(LTT[IntegrationCheck[Option]], LTT[IntegrationCheck[Id]])
    }

    "normalize stable PDTs (https://github.com/zio/zio/issues/3390)" in {
      val t1 = LTT[PDTNormA.Service]
      val t2 = LTT[PDTNormB.Service]
      assertSameStrict(t2, t1)
      assertDebugSame(t2, t1)

      val PDTAlias1 = PDTNormB
      val PDTAlias2 = PDTAlias1
      val PDTAlias3 = PDTAlias2
      val PDTAlias4 = PDTAlias3
      val PDTAlias5 = PDTAlias4
      val t3 = LTT[PDTAlias5.Service]
      assertSameStrict(t3, t1)
      assertDebugSame(t3, t1)
      object xa {
        val PDTAlias6 = PDTAlias5
      }

      val t4 = LTT[PDTNormA.type]
      val t5 = LTT[PDTAlias5.type]
      val t6 = LTT[xa.PDTAlias6.type]

      assertSameStrict(t5, t4)
      assertDebugSame(t5, t4)
      assertSameStrict(t6, t4)
      assertDebugSame(t6, t4)

      val literal = "x"
      val aliasLiteral: literal.type = literal
      val t7 = LTag[literal.type].tag
      val t8 = LTag[aliasLiteral.type].tag

      assertSameStrict(t7, t8)
      assertDebugSame(t7, t8)
    }

    "properly dealias and assign prefixes to existential types and wildcards" in {
      val withNothing = LTT[TestModel.With[Nothing]]
      val with_ = LTT[TestModel.With[_]]
      assert(withNothing.debug().contains(": izumi.reflect.test.TestModel::With[=scala.Nothing]"))
      assert(withNothing.debug().contains("- izumi.reflect.test.TestModel::With[=scala.Nothing]"))
      assert(with_.debug().contains(": izumi.reflect.test.TestModel::With[=?]"))
      assert(with_.debug().contains("- izumi.reflect.test.TestModel::With[=?]"))

      val list_ = LTT[List[_]]
      val immutableList_ = LTT[List[_]]
      assertChild(LTT[List[Int]], immutableList_)
      assertChild(LTT[scala.collection.immutable.List[Int]], list_)
      assertChild(list_, immutableList_)
      assertChild(immutableList_, list_)
      assertDebugSame(list_, immutableList_)
    }

    "no redundant $ in object names" in {
      val ltt = LTT[TestModel.BasicCases.BasicCase2.TestImpl0Good]
      assert(!ltt.debug().contains("BasicCase2$"))
      assert(!ltt.debug().contains("BasicCases$"))
      assert(!ltt.repr.contains("BasicCase2$"))
      assert(!ltt.repr.contains("BasicCases$"))
      assert(!ltt.toString.contains("BasicCase2$"))
      assert(!ltt.toString.contains("BasicCases$"))
      assert(ltt.longNameWithPrefix == "izumi.reflect.test.TestModel.BasicCases.BasicCase2.TestImpl0Good")
    }

    "support basic None.type subtype check" in {
      assertChild(LTT[None.type], LTT[Option[Int]])
    }

    "supports complex type lambdas" in {
      assertSame(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[λ[(A, B) => FM2[(B, A)]]])
      assertSame(
        `LTT[_[_]]`[({ type l[F[_]] = NestedTL2[W1, W2, F] })#l],
        `LTT[_[_]]`[({ type l[G[_]] = FM2[G[S[W2, W1]]] })#l]
      )
      assertChild(`LTT[_,_]`[NestedTL[Const, *, *]], `LTT[_,_]`[λ[(A, B) => FM2[(B, A)]]])
    }

    "intersection lambda tags should not contain junk bases" in {
      val tCtor = `LTT[_,_]`[T3]
      //      val tCtor = PlatformSpecific.fromRuntime(scala.reflect.runtime.universe.typeOf[T3[Any, Any]].typeConstructor)
      val debugCtor = tCtor.debug("ctor")

      val combined = tCtor.combine(LTT[Int], LTT[Boolean])
      val debugCombined = combined.debug("combined")

      val alias = LTT[T3[Int, Boolean]]
      val direct = LTT[W1 with W4[Boolean] with W5[Int]]

      println(debugCtor)
      println(debugCombined)
      println(alias.debug("alias"))
      println(direct.debug("direct"))

      assert(!debugCtor.contains("<refinement>"))
      assert(!debugCtor.contains("<none>"))
      assert(!debugCtor.contains("- T"))
      assert(!debugCtor.contains("W4[=B]"))
      assert(!debugCtor.contains("W3[=B]"))
      assert(!debugCtor.contains("W5[=A]"))

      assert(!direct.debug().contains("W4[=Int]"))
      assert(!direct.debug().contains("W4[=scala.Int]"))

      assert(!debugCombined.contains("<refinement>"))
      assert(!debugCombined.contains("<none>"))
      assert(!debugCombined.contains("- T"))
      assert(!debugCombined.contains("W4[=B]"))
      assert(!debugCombined.contains("W3[=B]"))
      assert(!debugCombined.contains("W5[=A]"))
      assert(debugCombined.contains("W5[=scala.Int]"))

      assertDebugSame(alias, direct)
    }
  }

}
