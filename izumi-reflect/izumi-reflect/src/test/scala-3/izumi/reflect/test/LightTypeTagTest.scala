/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.macrortti.*
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedNamedReference, Boundaries, Lambda}

import scala.collection.immutable.ListSet
import scala.collection.{BitSet, immutable, mutable}

class LightTypeTagTest extends SharedLightTypeTagTest {

  import TestModel._

  "lightweight type tags (Dotty)" should {

    "tautological intersections with Matchable are discarded from internal structure (Scala 3 specific, Matchable)" in {
      assertSameStrict(LTT[Matchable with Option[String]], LTT[Option[String]])
      assertDebugSame(LTT[Matchable with Option[String]], LTT[Option[String]])
    }

    "tautological intersections with Matchable are discarded from internal structure (Scala 3 specific, Matchable) (Tag)" in {
      assertSameStrict(Tag[Matchable with Option[String]].tag, LTT[Option[String]])
      assertDebugSame(Tag[Matchable with Option[String]].tag, LTT[Option[String]])
    }

    "tautological unions with Any/AnyRef/Matchable/Object are discarded from internal structure (Scala 3 specific, Matchable)" in {
      assertSameStrict(LTT[Any | Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Any])
      assertDebugSame(LTT[Any | Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Any])

      assertSameStrict(LTT[Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Matchable])
      assertDebugSame(LTT[Matchable | AnyRef | Object | Option[String] | Nothing], LTT[Matchable])

      assertSameStrict(LTT[AnyRef | Object | Option[String] | Nothing], LTT[AnyRef])
      assertDebugSame(LTT[AnyRef | Object | Option[String] | Nothing], LTT[AnyRef])

      assertSameStrict(LTT[Object | Option[String] | Nothing], LTT[Object])
      assertDebugSame(LTT[Object | Option[String] | Nothing], LTT[Object])

      assertSameStrict(LTT[Option[String] | Nothing], LTT[Option[String]])
      assertDebugSame(LTT[Option[String] | Nothing], LTT[Option[String]])
    }

    "tautological unions with Any/AnyRef/Matchable/Object are discarded from internal structure (Scala 3 specific, Matchable) (Tag)" in {
      assertSameStrict(Tag[Any | Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Any])
      assertDebugSame(Tag[Any | Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Any])

      assertSameStrict(Tag[Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Matchable])
      assertDebugSame(Tag[Matchable | AnyRef | Object | Option[String] | Nothing].tag, LTT[Matchable])

      assertSameStrict(Tag[AnyRef | Object | Option[String] | Nothing].tag, LTT[AnyRef])
      assertDebugSame(Tag[AnyRef | Object | Option[String] | Nothing].tag, LTT[AnyRef])

      assertSameStrict(Tag[Object | Option[String] | Nothing].tag, LTT[Object])
      assertDebugSame(Tag[Object | Option[String] | Nothing].tag, LTT[Object])

      assertSameStrict(Tag[Option[String] | Nothing].tag, LTT[Option[String]])
      assertDebugSame(Tag[Option[String] | Nothing].tag, LTT[Option[String]])
    }

    "support top-level abstract types (Scala 3 specific, top level type aliases)" in {
      assertChildStrict(LTT[LightTypeTagTestT], LTT[String])
    }

    "support opaque types" in {
      object x {
        type T >: List[Int] <: List[Int]
        opaque type Opaque = List[Int]
        opaque type OpaqueSub <: List[Int] = List[Int]
      }

      assertNotChildStrict(LTT[x.Opaque], LTT[List[Int]])
      assertNotChildStrict(LTT[x.Opaque], LTT[Seq[Int]])
      assertNotChildStrict(LTT[x.Opaque], LTT[x.T])
      assertNotChildStrict(LTT[x.Opaque], LTT[x.OpaqueSub])

      assertChildStrict(LTT[x.OpaqueSub], LTT[List[Int]])
      assertChildStrict(LTT[x.OpaqueSub], LTT[Seq[Int]])
      assertChildStrict(LTT[x.T], LTT[Seq[Int]])
      assertChildStrict(LTT[x.OpaqueSub], LTT[x.T])
      assertDifferent(LTT[x.OpaqueSub], LTT[x.T])
    }

    "basic support for polymorphic function types" in {
      val t1 = LTT[[A] => A => A]
      val t2 = LTT[[A] => A => A]
      assertSameStrict(t1, t2)
    }

    "support polymorphic function types with multiple type parameters" in {
      val t1 = LTT[[A, B] => A => B => (A, B)]
      val t2 = LTT[[A, B] => A => B => (A, B)]
      assertSameStrict(t1, t2)
    }

    "support polymorphic function types with bounds" in {
      trait Base
      val t1 = LTT[[A <: Base] => A => A]
      val t2 = LTT[[A <: Base] => A => A]
      assertSameStrict(t1, t2)
    }

    "support polymorphic function types with complex return types" in {
      val t1 = LTT[[A] => A => Option[A]]
      val t2 = LTT[[A] => A => Option[A]]
      assertSameStrict(t1, t2)
    }

    "support nested polymorphic function types" in {
      val t1 = LTT[[A] => A => [B] => B => Either[A, B]]
      val t2 = LTT[[A] => A => [B] => B => Either[A, B]]
      assertSameStrict(t1, t2)
    }

    "support polymorphic function types as type arguments" in {
       val t1 = LTT[List[[A] => A => A]]
       val t2 = LTT[List[[A] => A => A]]
       assertSameStrict(t1, t2)
    }

    "support structural types with polymorphic function members" in {
      type T1 = { def foo: [A] => A => A }
      type T2 = { def foo: [A] => A => A }
      
      val t1 = LTT[T1]
      val t2 = LTT[T2]
      assertSameStrict(t1, t2)
    }

    "support deeply nested polymorphic function types" in {
       // [A] => A => [B] => B => [C] => (A, B, C) => (C, B, A)
       val t1 = LTT[[A] => A => [B] => B => [C] => (A, B, C) => (C, B, A)]
       val t2 = LTT[[A] => A => [B] => B => [C] => (A, B, C) => (C, B, A)]
       assertSameStrict(t1, t2)
    }

    "support polymorphic functions taking polymorphic functions as arguments" in {
       // [A] => ( [X] => X => A ) => A
       val t1 = LTT[[A] => ([X] => X => A) => A]
       val t2 = LTT[[A] => ([X] => X => A) => A]
       assertSameStrict(t1, t2)
    }

    "support polymorphic function types with bounds and variance" in {
       trait Upper {}
       trait Lower extends Upper {}
       // [A <: Upper, B >: Lower] => A => B
       val t1 = LTT[[A <: Upper, B >: Lower] => A => B]
       val t2 = LTT[[A <: Upper, B >: Lower] => A => B]
        assertSameStrict(t1, t2)
    }

    "support polymorphic function types in intersection types" in {
      trait F1 { def f: [A] => A => A }
      trait F2 { def g: [B] => B => List[B] }
      
       val t1 = LTT[{ def f: [A] => A => A } & { def g: [B] => B => List[B] }]
       val t2 = LTT[{ def f: [A] => A => A } & { def g: [B] => B => List[B] }]
      
      assertSameStrict(t1, t2)
    }

    "support structural types with complex polymorphic members" in {
      type T1 = { def complex: [A] => List[A] => Option[A] }
      type T2 = { def complex: [A] => List[A] => Option[A] }
      
      assertSameStrict(LTT[T1], LTT[T2])
    }


  }
}

type LightTypeTagTestT <: String
