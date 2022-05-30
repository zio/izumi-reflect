package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.macrortti.LightTypeTagRef.{AppliedNamedReference, AppliedReference}

abstract class SharedLightTypeTagProgressionTest extends TagAssertions with TagProgressions {

  "[progression] lightweight type tags (all versions)" should {

    import TestModel._

    "progression test: Dotty fails to `support contravariance in refinement method comparisons`" in {
      doesntWorkYetOnDotty {
        assertDeepChild(LTT[{ def compare(a: AnyVal): Int }], LTT[{ def compare(b: Int): Int }])
      }
    }

    "properly dealias and assign prefixes to existential types and wildcards" in {
      val withNothing = LTT[With[Nothing]]
      val with_ = LTT[With[_]]
      doesntWorkYetOnDotty(assert(withNothing.debug().contains(": izumi.reflect.test.TestModel::With[=scala.Nothing]")))
      doesntWorkYetOnDotty(assert(withNothing.debug().contains("- izumi.reflect.test.TestModel::With[=scala.Nothing]")))
      doesntWorkYetOnDotty(assert(with_.debug().contains(": izumi.reflect.test.TestModel::With[=?]")))
      doesntWorkYetOnDotty(assert(with_.debug().contains("- izumi.reflect.test.TestModel::With[=?]")))

      val list_ = LTT[List[_]]
      val immutableList_ = LTT[List[_]]
      assertChild(LTT[List[Int]], immutableList_)
      assertChild(LTT[scala.collection.immutable.List[Int]], list_)
      assertChild(list_, immutableList_)
      assertChild(immutableList_, list_)
      assertDebugSame(list_, immutableList_)
    }

    "progression test: `support higher-kinded intersection type subtyping` isn't fully supported on Dotty" in {
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

      println(t1.debug("T1[_]"))
      println(t2.debug("T2[_]"))
      println(w3.debug("W3[_]"))
      println(w4.debug("W4[_]"))

      assertChild(t1, w3)
      assertChild(t1, LTT[W1])
      doesntWorkYetOnDotty {
        assertChild(w4, w3)
      }
      doesntWorkYetOnDotty {
        assertChild(t2, t1)
      }
    }

    "progression test: `support distinction between subtypes` doesn't work properly on Dotty" in {
      val strLTT = LTT[String]
      val subStrALTT = LTT[SubStrA]
      val subSubStrLTT = LTT[SubSubStr]
      val strUnpacker = LightTypeTagUnpacker(strLTT)
      val substrUnpacker = LightTypeTagUnpacker(subStrALTT)
      val subsubstrUnpacker = LightTypeTagUnpacker(subSubStrLTT)
      val strTR = strLTT.ref.asInstanceOf[AppliedNamedReference]
      val subStrTR = subStrALTT.ref.asInstanceOf[AppliedReference]
      val subSubStrTR = subSubStrLTT.ref.asInstanceOf[AppliedReference]

      doesntWorkYetOnDotty {
        assert(strUnpacker.bases.keySet == Set(strTR))
      }

      doesntWorkYetOnDotty {
        assert(subStrALTT.repr == "izumi.reflect.test.TestModel::SubStrA|<scala.Nothing..java.lang.String>")
      }
      observableIncorrectBehaviorOnDottyButNotOnScala2 {
        assert(subStrALTT.repr == "izumi.reflect.test.TestModel$::SubStrA|<scala.Nothing..java.lang.String>")
      }

      val nothingRef = LTT[Nothing].ref.asInstanceOf[AppliedNamedReference]
      val anyRef = LTT[Any].ref.asInstanceOf[AppliedNamedReference]

      doesntWorkYetOnDotty {
        assert(substrUnpacker.bases == strUnpacker.bases.map { case (s, v) if s.toString == "String" => subStrTR -> (v + strTR); case p => p })
      }
      observableIncorrectBehaviorOnDottyButNotOnScala2 {
        assert(substrUnpacker.bases == strUnpacker.bases + (nothingRef -> Set(anyRef)))
      }

      doesntWorkYetOnDotty {
        assert(subsubstrUnpacker.bases == strUnpacker.bases.map { case (strTR, v) => subSubStrTR -> (v + strTR) })
      }
      observableIncorrectBehaviorOnDottyButNotOnScala2 {
        assert(subsubstrUnpacker.bases == strUnpacker.bases + (nothingRef -> Set(anyRef)))
      }
    }

    "progression test: Dotty fails to `support human-readable representation` (should strip trailing $ from object names)" in {
      type TX[B] = Int { def a(k: String): Int; val b: String; type M1 = W1; type M2 <: W2; type M3[A] = Either[B, A] }
      doesntWorkYetOnDotty {
        assertRepr(
          `LTT[_]`[TX],
          "λ %0 → (Int & {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %2:0 → Either[+0,+2:0]})"
        )
      }
      doesntWorkYetOnDotty {
        assertRepr(
          `LTT[_]`[TX].combine(LTT[Unit]),
          "(Int & {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %2:0 → Either[+Unit,+2:0]})"
        )
      }
      doesntWorkYetOnDotty {
        assertRepr(
          LTT[TX[Unit]],
          "(Int & {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %1:0 → Either[+Unit,+1:0]})"
        )
      }
      doesntWorkYetOnDotty {
        assertRepr(LTT[I1 with (I1 with (I1 with W1))], "{TestModel::I1 & TestModel::W1}")
      }
      doesntWorkYetOnDotty {
        assertRepr(`LTT[_]`[R1], "λ %0 → TestModel::R1[=0]")
      }
      assertRepr(`LTT[_]`[Nothing], "Nothing")
      assertRepr(LTT[Int], "Int")
      assertRepr(LTT[List[Int]], "List[+Int]")
      assertRepr(LTT[Id[Int]], "Int")
      assertRepr(LTT[FP[Int]], "List[+Int]")
      doesntWorkYetOnDotty {
        assertRepr(`LTT[_]`[L], "λ %0 → List[+0]")
      }
      doesntWorkYetOnDotty {
        assertRepr(`LTT[_]`[Either[Unit, *]], "λ %0 → Either[+Unit,+0]")
      }
      doesntWorkYetOnDotty {
        assertRepr(`LTT[_]`[S[Unit, *]], "λ %0 → Either[+0,+Unit]")
      }
    }

    "progression test: what about non-empty refinements with intersections" in {
      val ltt = LTT[Int with Object with Option[String] { def a: Boolean }]
      println(ltt.debug())
      assert(!ltt.debug().contains("<refinement>"))
      assert(!ltt.debug().contains("* String"))
    }

    "progression test: can't support subtyping of type prefixes" in {
      val a = new C {}

      doesntWorkYetOnScala2 {
        assertChild(LTT[a.A], LTT[C#A])
      }
      doesntWorkYetOnDotty {
        assertDifferent(LTT[a.A], LTT[C#A])
        assertNotChild(LTT[C#A], LTT[a.A])
      }
    }

    "progression test: can't support subtyping of concrete type projections" in {
      trait A {
        trait T
      }
      trait B extends A

      val tagA = LTT[A#T]
      val tagB = LTT[B#T]

      assertSame(LTT[A#T], LTT[A#T])
      doesntWorkYetOnDotty {
        assertDifferent(LTT[B#T], LTT[A#T])
      }
      doesntWorkYetOnScala2 {
        assertChild(tagB, tagA)
      }
    }

    "progression test: subtype check fails when child type has absorbed a covariant type parameter of the supertype" in {
      assertChild(LTT[Set[Int]], LTT[Iterable[AnyVal]])

      val tagF3 = LTT[F3]
      val tagF2 = LTT[F2[Int]]
      assertChild(tagF3, tagF2)

      doesntWorkYetOnScala2 {
        assertChild(tagF3, LTT[F2[AnyVal]])
      }
    }

    "progression test: can't support equal-bounded types on Scala 2" in {
      object x {
        type X >: String <: String
      }
      val tag = LTT[String]
      val tag1 = LTT[x.X]

      doesntWorkYetOnScala2 {
        assertSameRef(tag, tag1)
        assertSame(tag, tag1)
      }
    }

    "progression test: fails to check subtyping when higher-kinds are involved on Scala 3" in {
      doesntWorkYetOnDotty {
        assertChild(LTT[FT2[IT2]], LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT2].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
        assertChild(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertDifferent(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT2]), LTT[FT1[IT1]])
        assertSame(`LTT[_[+_[_]]]`[FT1].combine(`LTT[_[+_]]`[IT1]), LTT[FT1[IT1]])
      }
    }

    "progression test: Dotty fails basic None.type subtype check" in {
      doesntWorkYetOnDotty {
        assertChild(LTT[None.type], LTT[Option[Int]])
      }
    }

    "progression test: Dotty fails some bounds-based subtype checks" in {
      // I consider this stuff practically useless
      type X[A >: H4 <: H2] = Option[A]
      doesntWorkYetOnDotty {
        assertNotChild(LTT[Option[H5]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
      }
//      // allTypeReferences: we need to use tpe.etaExpand but 2.13 has a bug: https://github.com/scala/bug/issues/11673#
//      doesntWorkYetOnScala2 {
      assertChild(LTT[Option[H3]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
//      }
    }

    "progression test: a portion of `support swapped parents` fails on Dotty" in {
      trait KK1[+A, +B, +U]
      trait KK2[+A, +B] extends KK1[B, A, Unit]

      doesntWorkYetOnDotty {
        assertChild(`LTT[_]`[KK2[H2, *]], `LTT[_]`[KK1[*, H1, Unit]])
      }
    }

    "progression test: a portion of `support subtyping of parents parameterized with type lambdas in combined tags` fails on Dotty" in {
      val childBase = `LTT[_[_,_]]`[RoleChild]
      val childArg = `LTT[_,_]`[Either]
      val combinedTag = childBase.combine(childArg)
      val parentTag = LTT[RoleParent[Either[Throwable, *]]]
      val childTag = LTT[RoleChild[Either]]

      assertChild(combinedTag, childTag)
      assertSame(combinedTag, childTag)

      doesntWorkYetOnDotty {
        assertChild(combinedTag, parentTag)
        assertNotChild(parentTag, combinedTag)
      }
    }

    "progression test: a portion of `support subtyping of parents parameterized with type lambdas in combined tags with multiple parameters` fails on Dotty" in {
      val childBase = `LTT[_[+_,+_],_,_]`[RoleChild2]
      val childArgs = Seq(`LTT[_,_]`[Either], LTT[Int], LTT[String])
      val combinedTag = childBase.combine(childArgs: _*)
      val expectedTag = LTT[RoleParent[Either[Throwable, *]]]
      val noncombinedTag = LTT[RoleChild2[Either, Int, String]]

      assertSame(combinedTag, noncombinedTag)
      assertChild(noncombinedTag, expectedTag)
      doesntWorkYetOnDotty {
        assertChild(combinedTag, expectedTag)
      }
    }

    "progression test: in `support literal types` literal encoding in Dotty version doesn't match Scala 2" in {
      doesntWorkYetOnDotty {
        assertRepr(literalLtt("str"), "\"str\"")
      }
    }

    "progression test: `support structural & refinement type subtype checks` doesn't work on Dotty" in {
      doesntWorkYetOnDotty {
        type C1 = C
        assertDeepSame(LTT[{ def a: Int }], LTT[{ val a: Int }])
        assertDeepSame(LTT[C { def a: Int }], LTT[C1 { def a: Int }])

        assertDeepChild(LTT[C { def a: Int }], LTT[C])
        assertDeepChild(LTT[C { type A = Int }], LTT[C])
        assertDeepChild(LTT[C { type A <: Int }], LTT[C])

        assertDeepChild(LTT[C { def a: Int; def b: Int }], LTT[C { def a: Int }])

        assertDeepChild(LTT[C { def a: Int }], LTT[{ def a: Int }])
      }
    }

    "progression test: `support structural subtype checks` doesn't work on Dotty" in {
      doesntWorkYetOnDotty {
        assertDeepChild(LTT[{ type T = List[Int] }], LTT[{ type T <: List[Any] }])
        assertDeepChild(LTT[{ type T = Int }], LTT[{ type T <: AnyVal }])
        assertDeepChild(LTT[{ type T = Int }], LTT[{ type T <: Any }])
        assertDeepChild(LTT[{ type T = String }], LTT[{ type T <: CharSequence }])
        assertDeepChild(LTT[{ def T: Int }], LTT[{ def T: AnyVal }])
        assertDeepChild(LTT[{ type T = Int }], LTT[{ type T <: AnyVal }])

        assertNotChild(LTT[{ type T = Int }], LTT[{ type T <: CharSequence }])
        assertDeepNotChild(LTT[{ def T: Int }], LTT[{ type T }])
      }
    }

    "progression test: indirect structural checks do not work" in {
      doesntWorkYetOnDotty {
        assertDifferent(LTT[{ type A }], LTT[Object])
      }
      doesntWorkYetOnScala2 {
        assertDeepChild(LTT[C], LTT[{ type A }])
      }
    }

    "progression test: Dotty fails to `support structural & refinement type equality`" in {
      doesntWorkYetOnDotty {
        assertDifferent(LTT[W4[str.type] with ({ type T = str.type with Int })], LTT[W4[str.type] with ({ type T = str.type with Long })])
      }

      type C1 = C
      assertSame(LTT[{ def a: Int }], LTT[{ def a: Int }])
      assertSame(LTT[C { def a: Int }], LTT[C1 { def a: Int }])

      assertDifferent(LTT[C { def a: Int }], LTT[{ def a: Int }])
      doesntWorkYetOnDotty {
        assertDifferent(LTT[C { def a: Int }], LTT[C])
      }

      doesntWorkYetOnDotty {
        assertDifferent(LTT[C { def a: Int }], LTT[C { def a: Int; def b: Int }])
      }
    }

    "progression test: `strong summons test` doesn't work on Dotty (LTag isn't checked for strength)" in {
      doesntWorkYetOnDotty {
        assertTypeError("def x1[T] = LTag[Array[T]]")
        assertTypeError("def x1[T] = LTag[Array[Int] { type X = T }]")
        assertTypeError("def x1[T <: { type Array }] = LTag[T#Array]")
        assertTypeError("def x1[T] = LTag[Array[Int] with List[T]]")
        assertTypeError("def x1[F[_]] = LTag[F[Int]]")
      }

      assertCompiles("def x1 = { object x { type T }; def x1 = LTag[Array[x.T]]; () }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] { type X = x.T }]; () }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] with List[x.T]]; () }")
      assertCompiles("def x1 = { object x { type F[_] }; LTag[x.F[Int]]; () }")
      assertCompiles("def x1 = { object x { type F[_[_]]; type Id[A] = A }; LTag[x.F[x.Id]]; () }")
    }

    "progression test: part of `normalize stable PDTs (https://github.com/zio/zio/issues/3390)` doesn't work on Dotty" in {
      val t1 = LTT[PDTNormA.Service]
      val t2 = LTT[PDTNormB.Service]
      assertSame(t2, t1)
      assertChild(t2, t1)
      assertChild(t1, t2)

      val PDTAlias1 = PDTNormB
      val PDTAlias2 = PDTAlias1
      val PDTAlias3 = PDTAlias2
      val PDTAlias4 = PDTAlias3
      val PDTAlias5 = PDTAlias4
      val t3 = LTT[PDTAlias5.Service]
      assertSame(t3, t1)
      assertChild(t3, t1)
      assertChild(t1, t3)

      val t4 = LTT[PDTNormA.type]
      val t5 = LTT[PDTAlias5.type]
      doesntWorkYetOnDotty {
        assertSame(t5, t4)
      }
      doesntWorkYetOnDotty {
        assertChild(t5, t4)
      }
      doesntWorkYetOnDotty {
        assertChild(t4, t5)
      }
      val literal = "x"
      val aliasLiteral: literal.type = literal
      val t6 = LTag[literal.type].tag
      val t7 = LTag[aliasLiteral.type].tag
      doesntWorkYetOnDotty {
        assertSame(t6, t7)
      }
      doesntWorkYetOnDotty {
        assertChild(t6, t7)
      }
      doesntWorkYetOnDotty {
        assertChild(t7, t6)
      }
    }

    "progression test: `support higher-kinded intersection type combination` isn't supported on Dotty" in {
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
      doesntWorkYetOnDotty {
        assertChild(combined, LTT[W3[Boolean]])
      }
      doesntWorkYetOnDotty {
        assertChild(combined, LTT[W1])
      }
      doesntWorkYetOnDotty {
        assertChild(combined, LTT[W2])
      }
      doesntWorkYetOnDotty {
        assertChild(combined, LTT[W1 with W3[Boolean]])
      }

      assertNotChild(combined, LTT[W4[Int]])
      assertNotChild(combined, LTT[W3[Int]])
      assertNotChild(combined, LTT[W5[Boolean]])
      assertNotChild(combined, LTT[W1 with W5[Boolean]])
    }

    "progression test: combined intersection lambda tags still contain some junk bases (coming from the unsound same-arity assumption in LightTypeTag#combine)" in {
      val tCtor = `LTT[_,_]`[T3]
      val combined = tCtor.combine(LTT[Int], LTT[Boolean])
      val debugCombined = combined.debug("combined")

      val alias = LTT[T3[Int, Boolean]]
      val direct = LTT[W1 with W4[Boolean] with W5[Int]]

      doesntWorkYetOnScala2 {
        assert(!debugCombined.contains("W4[=scala.Int]"))
      }
      doesntWorkYetOnScala2 {
        assert(!debugCombined.contains("W3[=scala.Int]"))
      }

      doesntWorkYet {
        assertDebugSame(combined, alias)
      }
      doesntWorkYet {
        assertDebugSame(combined, direct)
      }
    }

    "progression test: combined lambda tags still contain some junk bases (coming from the unsound same-arity assumption in LightTypeTag#combine)" in {
      val curriedApplied = `LTT[_,_]`[Right].combine(LTT[Throwable]).combine(LTT[Unit])
      val debug1 = curriedApplied.debug()

      assertSame(curriedApplied, LTT[Right[Throwable, Unit]])

      assert(debug1.contains(": scala.util.Right[+java.lang.Throwable,+scala.Unit]"))
      assert(debug1.contains("- scala.util.Right[+java.lang.Throwable,+scala.Unit]"))
      assert(debug1.contains("* scala.Product"))
      doesntWorkYetOnDotty {
        assert(debug1.contains("- λ %1 → scala.util.Right[+java.lang.Throwable,+1]"))
      }
      doesntWorkYetOnDotty {
        assert(debug1.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      }

      doesntWorkYetOnScala2 {
        assert(!debug1.contains("λ %1 → scala.util.Right[+scala.Unit,+1]"))
      }
    }

    "progression test: `applied tags should not contain junk bases` is not supported on Dotty" in {
      val debug0 = LTT[List[Any]].debug()
//      val debug0 = PlatformSpecific.fromRuntime[List[Any]].debug()

      assert(!debug0.contains("scala.List"))
      assert(!debug0.contains("package::List"))
      assert(!debug0.contains("<refinement>"))
      assert(!debug0.contains("<none>"))
      doesntWorkYetOnDotty {
        assert(debug0.contains("- λ %0 → scala.collection.immutable.List[+0]"))
      }

      //      val debug1 = LTT[List[_]].debug()
      val debug1 = PlatformSpecific.fromRuntime[List[_]].debug()

      assert(!debug1.contains("scala.List"))
      assert(!debug1.contains("package::List"))
      assert(!debug1.contains("<refinement>"))
      assert(!debug1.contains("<none>"))
      doesntWorkYetOnDotty {
        assert(debug1.contains("- λ %0 → scala.collection.immutable.List[+0]"))
      }

      val debug2 = LTT[Either[RoleChild[IO], Product]].debug()
//      val debug2 = PlatformSpecific.fromRuntime[Either[RoleChild[IO], Product]].debug()

      assert(!debug2.contains("package::Either"))
      assert(!debug2.contains("<refinement>"))
      assert(!debug2.contains("<none>"))
      assert(!debug2.contains("TestModel.E"))
      assert(!debug2.contains("TestModel.A"))
      doesntWorkYetOnDotty {
        assert(debug2.contains("- λ %0 → izumi.reflect.test.TestModel::RoleChild[=0]"))
      }
      doesntWorkYetOnDotty {
        assert(debug2.contains("* λ %0 → izumi.reflect.test.TestModel::RoleParent[=λ %1:0 → 0[=java.lang.Throwable,=1:0]]"))
      }
    }

    "progression test: `lambda tags should not contain junk bases` is not supported on Dotty" in {
      val debug1 = `LTT[_,_]`[Right].debug()

      assert(!debug1.contains("package::Either"))
      assert(!debug1.contains("scala.package.A"))
      assert(!debug1.contains("scala.package.B"))
      doesntWorkYetOnDotty {
        assert(debug1.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      }
      assert(debug1.contains("* scala.Product"))

      val debug2 = `LTT[_,_]`[Right].combine(LTT[Int], LTT[Int]).debug()

      assert(!debug2.contains("package::Either"))
      assert(!debug2.contains("scala.package.A"))
      assert(!debug2.contains("scala.package.B"))
      doesntWorkYetOnDotty {
        assert(debug2.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      }
      assert(debug2.contains("* scala.Product"))

      val debug3 = LTT[RoleParent[Right[Throwable, *]]].debug()
//      val debug3 = PlatformSpecific.fromRuntime[RoleParent[Right[Throwable, *]]].debug()

      assert(!debug3.contains("package::Right"))
      assert(!debug3.contains("<refinement>"))
      assert(!debug3.contains("<none>"))
      assert(!debug3.contains("TestModel.E"))
      assert(!debug3.contains("TestModel.A"))
      assert(!debug3.contains("+scala.Nothing"))
      doesntWorkYetOnDotty {
        assert(debug3.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      }
      assert(debug3.contains("* scala.Product"))

      val debug4 = `LTT[_]`[Right[Throwable, *]].debug()
//      val debug4 = PlatformSpecific
//        .fromRuntime(
//          scala.reflect.runtime.universe.typeOf[{ type l[a] = Right[Throwable, a] }].member(scala.reflect.runtime.universe.TypeName("l")).typeSignature
//        ).debug()

      assert(!debug4.contains("package::Right"))
      assert(!debug4.contains("<refinement>"))
      assert(!debug4.contains("<none>"))
      assert(!debug4.contains("TestModel.E"))
      assert(!debug4.contains("TestModel.A"))
      assert(!debug4.contains("+scala.Nothing"))
      doesntWorkYetOnDotty {
        assert(debug4.contains("- λ %0,%1 → scala.util.Right[+0,+1]"))
      }
      assert(debug4.contains("* scala.Product"))

      val oneArgApplied = `LTT[_,_]`[Right].combine(LTT[Throwable]).combine(LTT[Unit])
      val debug5 = oneArgApplied.debug()

      println(debug5)
      assert(!debug5.contains("package::Right"))
      assert(!debug5.contains("<refinement>"))
      assert(!debug5.contains("<none>"))
      assert(!debug5.contains("scala.package.A"))
      assert(!debug5.contains("scala.package.B"))
      assert(!debug5.contains("+scala.Nothing"))
      assert(debug5.contains("* scala.Product"))
    }

    "progression test: `intersection lambda tags should not contain junk bases` is not supported on Dotty" in {
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
      doesntWorkYetOnDotty {
        assert(!debugCtor.contains("W4[=B]"))
      }
      doesntWorkYetOnDotty {
        assert(!debugCtor.contains("W3[=B]"))
      }
      doesntWorkYetOnDotty {
        assert(!debugCtor.contains("W5[=A]"))
      }

      assert(!direct.debug().contains("W4[=Int]"))
      assert(!direct.debug().contains("W4[=scala.Int]"))

      assert(!debugCombined.contains("<refinement>"))
      assert(!debugCombined.contains("<none>"))
      assert(!debugCombined.contains("- T"))
      doesntWorkYetOnDotty {
        assert(!debugCombined.contains("W4[=B]"))
      }
      doesntWorkYetOnDotty {
        assert(!debugCombined.contains("W3[=B]"))
      }
      doesntWorkYetOnDotty {
        assert(!debugCombined.contains("W5[=A]"))
      }
      assert(debugCombined.contains("W5[=scala.Int]"))

      assertDebugSame(alias, direct)
    }

  }

}
