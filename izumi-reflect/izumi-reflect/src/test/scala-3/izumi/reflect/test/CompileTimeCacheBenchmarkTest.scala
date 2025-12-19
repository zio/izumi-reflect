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

import izumi.reflect.*
import izumi.reflect.macrortti.*
import org.scalatest.wordspec.AnyWordSpec

/** Compile-time cache correctness test for Scala 3. */
class CompileTimeCacheBenchmarkTest extends AnyWordSpec with TagAssertions {

  "Compile-time cache" should {
    
    "produce identical Tags for repeated materializations of the same simple type" in {
      val tag1 = Tag[String]
      val tag2 = Tag[String]
      val tag3 = Tag[String]
      
      assertSameStrict(tag1.tag, tag2.tag)
      assertSameStrict(tag2.tag, tag3.tag)
      assertSameStrict(tag1.tag, tag3.tag)
    }
    
    "produce identical Tags for repeated materializations of generic types" in {
      val tag1 = Tag[List[Int]]
      val tag2 = Tag[List[Int]]
      
      assertSameStrict(tag1.tag, tag2.tag)
      
      val tag3 = Tag[Map[String, Int]]
      val tag4 = Tag[Map[String, Int]]
      
      assertSameStrict(tag3.tag, tag4.tag)
    }
    
    "produce identical Tags for complex nested generic types" in {
      val tag1 = Tag[Either[List[Option[Int]], Map[String, Set[Long]]]]
      val tag2 = Tag[Either[List[Option[Int]], Map[String, Set[Long]]]]
      
      assertSameStrict(tag1.tag, tag2.tag)
    }
    
    "correctly handle types sharing common parent types (inheritance DB caching)" in {
      val listTag = Tag[List[Int]]
      val vectorTag = Tag[Vector[Int]]
      
      // Verify they are different types
      assertDifferent(listTag.tag, vectorTag.tag)
      
      val iterableTag = Tag[Iterable[Int]]
      assertChild(listTag.tag, iterableTag.tag)
      assertChild(vectorTag.tag, iterableTag.tag)
    }
    
    "correctly handle higher-kinded types" in {
      val tagK1 = TagK[List]
      val tagK2 = TagK[List]
      
      assertSameStrict(tagK1.tag, tagK2.tag)
      
      val tagKK1 = TagKK[Either]
      val tagKK2 = TagKK[Either]
      
      assertSameStrict(tagKK1.tag, tagKK2.tag)
    }
    
    "correctly handle intersection types" in {
      trait A
      trait B
      trait C
      
      val tag1 = Tag[A & B]
      val tag2 = Tag[A & B]
      
      assertSameStrict(tag1.tag, tag2.tag)
      
      val tag3 = Tag[A & B & C]
      val tag4 = Tag[A & B & C]
      
      assertSameStrict(tag3.tag, tag4.tag)
    }
    
    "correctly handle union types" in {
      val tag1 = Tag[Int | String]
      val tag2 = Tag[Int | String]
      
      assertSameStrict(tag1.tag, tag2.tag)
    }
    
    "correctly handle parameterized types in methods" in {
      def makeTag[T: Tag]: Tag[T] = Tag[T]
      def makeListTag[T: Tag]: Tag[List[T]] = Tag[List[T]]
      
      val intTag1 = makeTag[Int]
      val intTag2 = makeTag[Int]
      assertSameStrict(intTag1.tag, intTag2.tag)
      
      val listIntTag1 = makeListTag[Int]
      val listIntTag2 = makeListTag[Int]
      assertSameStrict(listIntTag1.tag, listIntTag2.tag)
    }
    
    "demonstrate cache benefit with types sharing structure" in {
      val tag1 = Tag[List[Option[Int]]]
      val tag2 = Tag[Set[Option[Int]]]
      val tag3 = Tag[Vector[Option[Int]]]
      
      val optionIntTag = Tag[Option[Int]]
      
      assertChild(tag1.tag, Tag[Iterable[Option[Int]]].tag)
      assertChild(tag2.tag, Tag[Iterable[Option[Int]]].tag)
      assertChild(tag3.tag, Tag[Iterable[Option[Int]]].tag)
    }
    
    "handle repeated complex type combinations" in {
      type ServiceResult[A] = Either[Throwable, A]
      type AsyncResult[A] = ServiceResult[Option[A]]
      
      val tag1 = Tag[AsyncResult[Int]]
      val tag2 = Tag[AsyncResult[Int]]
      val tag3 = Tag[AsyncResult[String]]
      
      assertSameStrict(tag1.tag, tag2.tag)
      assertDifferent(tag1.tag, tag3.tag)
      
      val tagDirect = Tag[Either[Throwable, Option[Int]]]
      assertSameStrict(tag1.tag, tagDirect.tag)
    }
  }
  
  "Cache correctness verification" should {
    
    "maintain type equality semantics across cache operations" in {
      val tags = (1 to 5).map(_ => Tag[Map[String, List[Int]]])
      
      for {
        t1 <- tags
        t2 <- tags
      } {
        assertSameStrict(t1.tag, t2.tag)
      }
    }
    
    "correctly distinguish between similar but different types" in {
      val tag1 = Tag[List[Int]]
      val tag2 = Tag[List[Long]]
      val tag3 = Tag[List[String]]
      val tag4 = Tag[Vector[Int]]
      
      assertDifferent(tag1.tag, tag2.tag)
      assertDifferent(tag1.tag, tag3.tag)
      assertDifferent(tag1.tag, tag4.tag)
      assertDifferent(tag2.tag, tag3.tag)
    }
    
    "handle type bounds correctly" in {
      def upperBounded[T <: AnyVal: Tag]: Tag[T] = Tag[T]
      def lowerBounded[T >: Null: Tag]: Tag[T] = Tag[T]
      
      val intTag1 = upperBounded[Int]
      val intTag2 = upperBounded[Int]
      assertSameStrict(intTag1.tag, intTag2.tag)
      
      val stringTag1 = lowerBounded[String]
      val stringTag2 = lowerBounded[String]
      assertSameStrict(stringTag1.tag, stringTag2.tag)
    }
    
    "handle path-dependent types" in {
      class Outer {
        class Inner
      }
      
      val outer1 = new Outer
      val outer2 = new Outer
      
      val tag1a = Tag[outer1.Inner]
      val tag1b = Tag[outer1.Inner]
      assertSameStrict(tag1a.tag, tag1b.tag)
      
      val tag2 = Tag[outer2.Inner]
      assertDifferent(tag1a.tag, tag2.tag)
    }
    
    "handle singleton types" in {
      object MySingleton
      
      val tag1 = Tag[MySingleton.type]
      val tag2 = Tag[MySingleton.type]
      assertSameStrict(tag1.tag, tag2.tag)
    }
    
    "handle literal types" in {
      val tag1 = Tag[1]
      val tag2 = Tag[1]
      val tag3 = Tag[2]
      
      assertSameStrict(tag1.tag, tag2.tag)
      assertDifferent(tag1.tag, tag3.tag)
      
      val tagStr1 = Tag["hello"]
      val tagStr2 = Tag["hello"]
      val tagStr3 = Tag["world"]
      
      assertSameStrict(tagStr1.tag, tagStr2.tag)
      assertDifferent(tagStr1.tag, tagStr3.tag)
    }
  }
}
