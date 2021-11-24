package izumi.reflect.test

import izumi.reflect.macrortti._
import izumi.reflect.macrortti.LightTypeTagRef.AppliedNamedReference

abstract class SharedLightTypeTagProgressionTest extends TagAssertions {

  "lightweight type tags (including Dotty)" should {

    import TestModel._

    "progression test: `support distinction between subtypes` doesn't work properly on Dotty" in {
      val strLTT = LTT[String]
      val subStrALTT = LTT[SubStrA]
      val subSubStrLTT = LTT[SubSubStr]
      val strUnpacker = LightTypeTagUnpacker(strLTT)
      val substrUnpacker = LightTypeTagUnpacker(subStrALTT)
      val subsubstrUnpacker = LightTypeTagUnpacker(subSubStrLTT)
      val strTR = strLTT.ref.asInstanceOf[AppliedNamedReference]
      val subStrTR = subStrALTT.ref.asInstanceOf[AppliedNamedReference]
      val subSubStrTR = subSubStrLTT.ref.asInstanceOf[AppliedNamedReference]

      doesntWorkYetOnDotty {
        assert(strUnpacker.bases.keySet == Set(strTR))
      }

      doesntWorkYetOnDotty {
        assert(subStrALTT.repr == "izumi.reflect.test.TestModel::izumi.reflect.test.TestModel.SubStrA|<scala.Nothing..java.lang.String>")
      }
      observableIncorrectBehaviorOnDottyButNotOnScala2 {
        assert(subStrALTT.repr == "izumi.reflect.test.TestModel$::izumi.reflect.test.TestModel$.SubStrA|<scala.Nothing..java.lang.String>")
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
      doesntWorkYetOnDotty {
        assertRepr(
          LTT[Int { def a(k: String): Int; val b: String; type M1 = W1; type M2 <: W2; type M3[A] = Either[Unit, A] }],
          "(Int & {def a(String): Int, def b(): String, type M1 = TestModel::W1, type M2 = M2|<Nothing..TestModel::W2>, type M3 = λ %0 → Either[+Unit,+0]})"
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

    "progression test: tautological intersections with Any/Object are still preserved in internal structure despite being useless" in {
      doesntWorkYet {
        assertDebugSame(LTT[Object with Option[String]], LTT[Option[String]])
        assertDebugSame(LTT[Any with Option[String]], LTT[Option[String]])
        assertDebugSame(LTT[AnyRef with Option[String]], LTT[Option[String]])
      }
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

    "progression test: wildcards are not supported" in {
      doesntWorkYet {
        assertChild(LTT[Set[Int]], LTT[Set[_]])
      }
    }

    "progression test: subtype check fails when child type has absorbed a covariant type parameter of the supertype" in {
      assertChild(LTT[Set[Int]], LTT[Iterable[AnyVal]])

      val tagF3 = LTT[F3]
      assertChild(tagF3, LTT[F2[Int]])

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

    "progression test: fails None.type subtype check" in {
      doesntWorkYetOnDotty {
        assertChild(LTT[None.type], LTT[Option[Int]])
      }
    }

    "progression test: fails some bounds-based subtype checks" in {
      // I consider this stuff practically useless
      type X[A >: H4 <: H2] = Option[A]
      doesntWorkYetOnDotty {
        assertNotChild(LTT[Option[H5]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
      }
      // allTypeReferences: we need to use tpe.etaExpand but 2.13 has a bug: https://github.com/scala/bug/issues/11673#
      doesntWorkYetOnScala2 {
        assertChild(LTT[Option[H3]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
      }
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
      val expectedTag = LTT[RoleParent[Either[Throwable, *]]]

      assertSame(combinedTag, LTT[RoleChild[Either]])
      doesntWorkYetOnDotty {
        assertChild(combinedTag, expectedTag)
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
        assertChild(LTT[{ def a: Int }], LTT[{ def a: Int }])
        assertChild(LTT[C { def a: Int }], LTT[C1 { def a: Int }])

        assertChild(LTT[C { def a: Int }], LTT[C])
        assertNotChild(LTT[C], LTT[C { def a: Int }])

        assertChild(LTT[C { def a: Int; def b: Int }], LTT[C { def a: Int }])
        assertNotChild(LTT[C { def a: Int }], LTT[C { def a: Int; def b: Int }])

        assertChild(LTT[C { def a: Int }], LTT[{ def a: Int }])
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

  }

}
