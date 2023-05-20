package izumi.reflect.test

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, Boundaries, FullReference, NameReference, TypeParam}
import izumi.reflect.macrortti._

import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

abstract class SharedLightTypeTagTest extends TagAssertions {

  import TestModel._

  "lightweight type tags (all versions)" should {

    "support distinction between subtypes" in {
      val str = LTT[String]
      val subStrA = LTT[SubStrA]
      val subStrB = LTT[SubStrB]
      val subStrC = LTT[SubStrC]
      val subStrD = LTT[SubStrD]
      val subSubStr = LTT[SubSubStr]

      assertChildStrict(subStrA, str)
      assertChildStrict(subSubStr, str)
      assertChildStrict(subSubStr, subStrA)
      assertNotChildStrict(subSubStr, subStrB)

      assertSameStrict(subStrC, str)

      assertNotChild(subStrA, subStrB)
      assertSame(subStrA, subStrD)

      // see https://github.com/7mind/izumi/pull/1528
      assertSame(str.combine(), str)

      val strInhBases = LightTypeTagUnpacker(str).inheritance(str.ref.asInstanceOf[NameReference])
      val subStrAInhBases = LightTypeTagUnpacker(subStrA).inheritance(subStrA.ref.asInstanceOf[NameReference].copy(boundaries = Boundaries.Empty))
      val subSubStrInhBases = LightTypeTagUnpacker(subSubStr).inheritance(subSubStr.ref.asInstanceOf[NameReference].copy(boundaries = Boundaries.Empty))

      assert(subStrAInhBases == (strInhBases ++ Set(str.ref.asInstanceOf[NameReference])))
      assert(subSubStrInhBases == (strInhBases ++ Set(str.ref.asInstanceOf[NameReference])))

      val strFullBases = LightTypeTagUnpacker(str).bases(str.ref.asInstanceOf[NameReference])
      val subStrAFullBases = LightTypeTagUnpacker(subStrA).bases(subStrA.ref.asInstanceOf[NameReference])
      val subSubStrFullBases = LightTypeTagUnpacker(subSubStr).bases(subSubStr.ref.asInstanceOf[NameReference])

      assert(LightTypeTagUnpacker(str).bases.keySet == Set(str.ref.asInstanceOf[AppliedNamedReference]))
      assert(subStrA.repr == "izumi.reflect.test.TestModel::SubStrA|<scala.Nothing..java.lang.String>")
      assert(subStrA.repr != "izumi.reflect.test.TestModel$::SubStrA|<scala.Nothing..java.lang.String>")

      assert(subStrAFullBases == (strFullBases ++ Set(str.ref.asInstanceOf[AbstractReference])))
      assert(subSubStrFullBases == (strFullBases ++ Set(str.ref.asInstanceOf[AbstractReference])))

      val foo = LTT[SubStrA \/ Int]
      val bar = `LTT[_,_]`[\/].combine(subStrA, LTT[Int])
      assertSameStrict(foo, bar)
    }

    "eradicate intersection tautologies with Any/Object" in {
      assertSameStrict(LTT[Any with Option[String]], LTT[Option[String]])
      assertSameStrict(LTT[AnyRef with Option[String]], LTT[Option[String]])
      assertSameStrict(LTT[Object with Option[String]], LTT[Option[String]])
    }

    "do not eradicate intersections with Nothing" in {
      assertDifferent(LTT[Nothing with Option[String]], LTT[Option[String]])
      assertSameStrict(LTT[Nothing with Option[String]], LTT[Option[String] with Nothing])
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
      val t1 = LTT[XS]
      val t2 = LTT[WithX]
      val t3 = LTT[{ type X >: Nothing <: Any }]
      val t4 = LTT[{ type X >: Any <: Any }]
      val t5 = LTT[{ type X = Any }]
      val t6 = LTT[{ type X = Int }]
      val t7 = LTT[{ type X <: AnyVal }]

      assertChildStrict(t1, t2)
      assertChildStrict(t1, t3)
      assertNotChild(t3, t4)
      assertChildStrict(t4, t3)
      assertChildStrict(t5, t3)
      assertSameStrict(t3, LTT[{ type X }])
      assertNotChild(t3, t7)

      assertNotChild(t3, t5)
      assertNotChild(t2, t5)
      assertNotChild(t3, t4)
      assertNotChild(t2, t4)
      assertNotChild(t1, t4)

      assertChildStrict(t6, t3)
      assertChildStrict(t6, t7)
      assertNotChildStrict(t6, t4)
      assertNotChildStrict(t6, t5)
    }

    "support refinement higher-kinded subtype checks" in {
      val t1 = LTT[{ type F[A] = A }]
      val t2 = LTT[{ type F[A] <: Any }]
      val t3 = LTT[{ type F[A] <: AnyVal }]
      val t4 = LTT[FXS]

      assertChildStrict(t1, t2)
      assertNotChildStrict(t1, t3)

      assertChildStrict(t4, t1)
      assertChildStrict(t4, t2)
      assertNotChildStrict(t4, t3)
    }

    "support literal types" in {
      assertSame(literalLtt("str2"), literalLtt("str2"))
      assertDifferent(literalLtt("str1"), literalLtt("str2"))

      assertChildStrict(literalLtt("str"), LTT[String])
      assertNotChild(literalLtt("str"), LTT[Int])

      val tag = literalLtt("str")
      assertRepr(tag, "\"str\"")
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
      assertChild(LTT[Set[Int]], LTT[Set[_]])
      assertNotChild(LTT[Set[_]], LTT[Set[Int]])
      assertChild(LTT[Set[Any]], LTT[Set[_]])
      assertNotChild(LTT[Set[_]], LTT[Set[Any]])

      assertDifferent(LTT[List[_]], LTT[List[Any]])
      assertChild(LTT[List[Int]], LTT[List[_]])
      assertNotChild(LTT[List[_]], LTT[List[Int]])
      assertChild(LTT[List[Any]], LTT[List[_]])
      assertChild(LTT[List[_]], LTT[List[Any]])

      assertDifferent(LTT[Int => Int], LTT[_ => Int])
      assertChild(LTT[Int => Int], LTT[_ => Int])
      assertChild(LTT[_ => Int], LTT[Int => Int]) // incorrect but whatever
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

    "No degenerate lambdas (regression test https://github.com/zio/izumi-reflect/issues/345)" in {
      val fullDb = LTT[List[Int]].basesdb

      fullDb.foreach {
        case (_, parents) =>
          parents.foreach {
            case LightTypeTagRef.Lambda(List(name), FullReference(_, params, _)) =>
              assert(params.exists {
                case TypeParam(NameReference(ref, _, _), _) =>
                  name == ref
                case _ => false
              })
            case _ =>
          }
      }
    }

    "check subtyping when higher-kinds are involved on Scala 3" in {
      assertChild(LTT[FT2[IT2]], LTT[FT1[IT1]])
      assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
      assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
      assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
      assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
      assertChild(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
      assertDifferent(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
      assertSame(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
    }

    "support higher-kinded intersection type subtyping" in {
      type F1 = W3[Int] with W1
      type F2 = W4[Int] with W2

      type T1[A] = W3[A] with W1
      type T2[A] = W4[A] with W2

      val f1 = LTT[F1]
      val f2 = LTT[F2]

      assertChild(f1, LTT[W3[Int]])
      assertChild(f1, LTT[W1])
      assertChild(f2, f1)

      val t1 = `LTT[_]`[T1]
      val t2 = `LTT[_]`[T2]
      val w3 = `LTT[_]`[W3]
      val w4 = `LTT[_]`[W4]

      assertChild(t1, w3)
      assertChild(t1, LTT[W1])
      assertChild(w4, w3)
      assertChild(t2, t1)
    }

    "support higher-kinded intersection type combination isn't supported on Dotty" in {
      val tCtor = `LTT[_,_]`[T3]

      val combined = tCtor.combine(LTT[Int], LTT[Boolean])
      val alias = LTT[T3[Int, Boolean]]
      val direct = LTT[W1 with W4[Boolean] with W5[Int]]

      assertChild(alias, direct)
      assertChild(combined, alias)
      assertChild(combined, direct)

      assertSame(alias, direct)
      assertSame(alias, combined)

      assertDifferent(combined, LTT[Either[Int, Boolean]])
      assertDifferent(combined, LTT[T3[Boolean, Int]])

      assertNotChild(combined, LTT[Either[Int, Boolean]])
      assertNotChild(combined, LTT[T3[Boolean, Int]])

      assertChild(combined, LTT[W5[Int]])
      assertChild(combined, LTT[W4[Boolean]])
      assertChild(combined, LTT[W3[Boolean]])
      assertChild(combined, LTT[W1])
      assertChild(combined, LTT[W2])
      assertChild(combined, LTT[W1 with W3[Boolean]])

      assertNotChild(combined, LTT[W4[Int]])
      assertNotChild(combined, LTT[W3[Int]])
      assertNotChild(combined, LTT[W5[Boolean]])
      assertNotChild(combined, LTT[W1 with W5[Boolean]])
    }

    "support structural & refinement type subtype checks" in {
      type C1 = C
      assertSameStrict(LTT[{ def a: Int }], LTT[{ val a: Int }])
      assertSameStrict(LTT[C { def a: Int }], LTT[C1 { def a: Int }])

      assertChildStrict(LTT[C { def a: Int }], LTT[C])
      assertChildStrict(LTT[C { type A = Int }], LTT[C])
      assertChildStrict(LTT[C { type A <: Int }], LTT[C])

      assertChildStrict(LTT[C { def a: Int; def b: Int }], LTT[C { def a: Int }])

      assertChildStrict(LTT[C { def a: Int }], LTT[{ def a: Int }])
    }

    "support structural & refinement type equality" in {
      assertDifferent(LTT[W4[str.type] with ({ type T = str.type with Int })], LTT[W4[str.type] with ({ type T = str.type with Long })])

      type C1 = C
      assertSame(LTT[{ def a: Int }], LTT[{ def a: Int }])
      assertSame(LTT[C { def a: Int }], LTT[C1 { def a: Int }])

      assertDifferent(LTT[C { def a: Int }], LTT[{ def a: Int }])
      assertDifferent(LTT[C { def a: Int }], LTT[C])

      assertDifferent(LTT[C { def a: Int }], LTT[C { def a: Int; def b: Int }])
    }

    "strong summons test" in {
      assertTypeError("def x1[T] = LTag[Array[Int] { type X = T }]")

      assertTypeError("def x1[T] = LTag[Array[T]]")
      assertTypeError("def x1[T <: { type Array }] = LTag[T#Array]")
      assertTypeError("def x1[T] = LTag[Array[Int] with List[T]]")
      assertTypeError("def x1[F[_]] = LTag[F[Int]]")

      assertCompiles("def x1 = { object x { type T }; def x1 = LTag[Array[x.T]]; () }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] { type X = x.T }]; () }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] with List[x.T]]; () }")
      assertCompiles("def x1 = { object x { type F[_] }; LTag[x.F[Int]]; () }")
      assertCompiles("def x1 = { object x { type F[_[_]]; type Id[A] = A }; LTag[x.F[x.Id]]; () }")
    }

    "distinguishes between val and type structural refinements" in {
      val t1 = LTT[{ type T = Either[Int, String] }]
      val t2 = LTT[{ val T: Either[Int, String] }]
      assertNotChildStrict(t1, t2)
    }

    "does not contain intersections in plain structural refinements" in {
      val t1 = LTT[Any { type T = Int }]
      val t2 = LTT[{ def x: Int }]
      assert(!t1.debug().contains("&"))
      assert(!t2.debug().contains("&"))
    }

    "support equal-bounded types as paradoxical (before 2.3.0 and since 2.3.6 NOT equal to their underlying)" in {
      object x {
        type X >: String <: String
      }
      val tag = LTT[String]
      val tag1 = LTT[x.X]

      // equal bounds create a paradox where s <:< t && t <:< s but not s == t, because refs are not the same.
      // but this is also technically true in Scala. equal bounded abstract types are not identical - have their own
      // implicit scope, etc. And because in practical usage it's useful to permit these abstract types we're fine with
      // representing them despite them breaking our model.
      assertChild(tag, tag1)
      assertChild(tag1, tag)
      assertDifferent(tag1, tag) // paradox and bad, but also an inevitable result of using "optimistic equality" with binary strings
    }

    "support structural subtype checks" in {
      assertNotChildStrict(LTT[{ type T = List[Int] }], LTT[{ type T[A] = List[A] }])
      assertChildStrict(LTT[{ type T = List[Int] }], LTT[{ type T <: List[Any] }])
      assertChildStrict(LTT[{ type T = Int }], LTT[{ type T <: AnyVal }])
      assertChildStrict(LTT[{ type T = Int }], LTT[{ type T <: Any }])
      assertChildStrict(LTT[{ type T = String }], LTT[{ type T <: CharSequence }])
      assertChildStrict(LTT[{ def T: Int }], LTT[{ def T: AnyVal }])
      assertChildStrict(LTT[{ type T = Int }], LTT[{ type T <: AnyVal }])

      assertNotChild(LTT[{ type T = Int }], LTT[{ type T <: CharSequence }])
      assertNotChildStrict(LTT[{ def T: Int }], LTT[{ type T }])
    }

    "what about non-empty refinements with intersections" in {
      val ltt = LTT[Int with Object with Option[String] { def a: Boolean }]
      val debug = ltt.debug()
      assert(!debug.contains("<refinement>"))
      assert(!debug.contains("<none>"))
      assert(!debug.contains("* String"))
      assert(debug.contains("- java.lang.String"))
    }

    "support contravariance in refinement method comparisons" in {
      val t1 = LTT[{ def compare(a: AnyVal): Int }]
      val t2 = LTT[{ def compare(b: Int): Int }]
      assertChildStrict(t1, t2)
    }

    "support human-readable representation" in {
      type TX[B] = Int { def a(k: String): Int; val b: String; type M1 = W1; type M2 <: W2; type M3[A] = Either[B, A] }
      val txTag = `LTT[_]`[TX]
      assert(
        (txTag.toString // Scala 2
        == "λ %0 → (Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %2:0 → Either[+0,+2:0]})")
        || (txTag.toString // Dotty
        == "λ %0 → (Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %1:0 → Either[+0,+1:0]})")
      )
      val txCombinedTag = `LTT[_]`[TX].combine(LTT[Unit])
      assert(
        (txCombinedTag.toString // Scala 2
        == "(Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %2:0 → Either[+Unit,+2:0]})")
        || (txCombinedTag.toString // Dotty
        == "(Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %1:0 → Either[+Unit,+1:0]})")
      )
      val txUnitTag = LTT[TX[Unit]]
      assert(
        (txUnitTag.toString
        == "(Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %1:0 → Either[+Unit,+1:0]})")
        || (txUnitTag.toString
        == "(Int {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %0 → Either[+Unit,+0]})")
      )
      assertRepr(LTT[I1 with (I1 with (I1 with W1))], "{TestModel::I1 & TestModel::W1}")
      assertRepr(`LTT[_]`[R1], "λ %0 → TestModel::R1[=0]")
      assertRepr(`LTT[_]`[Nothing], "Nothing")
      assertRepr(LTT[Int], "Int")
      assertRepr(LTT[List[Int]], "List[+Int]")
      assertRepr(LTT[Id[Int]], "Int")
      assertRepr(LTT[FP[Int]], "List[+Int]")
      assertRepr(`LTT[_]`[L], "λ %0 → List[+0]")
      assertRepr(`LTT[_]`[Either[Unit, *]], "λ %0 → Either[+Unit,+0]")
      assertRepr(`LTT[_]`[S[Unit, *]], "λ %0 → Either[+0,+Unit]")
    }

  }

}
