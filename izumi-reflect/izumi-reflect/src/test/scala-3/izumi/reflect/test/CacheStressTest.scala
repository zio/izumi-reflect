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

/** Stress test for compile-time caching with many repeated Tag materializations. */
class CacheStressTest extends AnyWordSpec with TagAssertions {

  type ComplexType = Either[
    List[Option[Map[String, Set[Int]]]],
    Vector[Either[Option[Long], List[Map[String, Either[Int, String]]]]]
  ]

  "Cache stress test with repeated identical types" should {
    
    "handle repeated complex type 1-10" in {
      val t1 = Tag[ComplexType]; val t2 = Tag[ComplexType]; val t3 = Tag[ComplexType]
      val t4 = Tag[ComplexType]; val t5 = Tag[ComplexType]; val t6 = Tag[ComplexType]
      val t7 = Tag[ComplexType]; val t8 = Tag[ComplexType]; val t9 = Tag[ComplexType]
      val t10 = Tag[ComplexType]
      val tags = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle repeated complex type 11-20" in {
      val t11 = Tag[ComplexType]; val t12 = Tag[ComplexType]; val t13 = Tag[ComplexType]
      val t14 = Tag[ComplexType]; val t15 = Tag[ComplexType]; val t16 = Tag[ComplexType]
      val t17 = Tag[ComplexType]; val t18 = Tag[ComplexType]; val t19 = Tag[ComplexType]
      val t20 = Tag[ComplexType]
      val tags = List(t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle repeated complex type 21-30" in {
      val t21 = Tag[ComplexType]; val t22 = Tag[ComplexType]; val t23 = Tag[ComplexType]
      val t24 = Tag[ComplexType]; val t25 = Tag[ComplexType]; val t26 = Tag[ComplexType]
      val t27 = Tag[ComplexType]; val t28 = Tag[ComplexType]; val t29 = Tag[ComplexType]
      val t30 = Tag[ComplexType]
      val tags = List(t21, t22, t23, t24, t25, t26, t27, t28, t29, t30)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle repeated complex type 31-40" in {
      val t31 = Tag[ComplexType]; val t32 = Tag[ComplexType]; val t33 = Tag[ComplexType]
      val t34 = Tag[ComplexType]; val t35 = Tag[ComplexType]; val t36 = Tag[ComplexType]
      val t37 = Tag[ComplexType]; val t38 = Tag[ComplexType]; val t39 = Tag[ComplexType]
      val t40 = Tag[ComplexType]
      val tags = List(t31, t32, t33, t34, t35, t36, t37, t38, t39, t40)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle repeated complex type 41-50" in {
      val t41 = Tag[ComplexType]; val t42 = Tag[ComplexType]; val t43 = Tag[ComplexType]
      val t44 = Tag[ComplexType]; val t45 = Tag[ComplexType]; val t46 = Tag[ComplexType]
      val t47 = Tag[ComplexType]; val t48 = Tag[ComplexType]; val t49 = Tag[ComplexType]
      val t50 = Tag[ComplexType]
      val tags = List(t41, t42, t43, t44, t45, t46, t47, t48, t49, t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }

  type DeepType1 = Map[String, List[Option[Either[Int, Map[Long, Set[String]]]]]]
  type DeepType2 = List[Vector[Set[Map[String, Option[Either[Int, Long]]]]]]
  type DeepType3 = Either[Map[String, List[Int]], Vector[Set[Option[Long]]]]

  "Cache stress test with multiple different complex types" should {

    "handle DeepType1 repeated 20 times" in {
      val tags = List(
        Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1],
        Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1],
        Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1],
        Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1], Tag[DeepType1]
      )
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle DeepType2 repeated 20 times" in {
      val tags = List(
        Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2],
        Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2],
        Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2],
        Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2], Tag[DeepType2]
      )
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }

    "handle DeepType3 repeated 20 times" in {
      val tags = List(
        Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3],
        Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3],
        Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3],
        Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3], Tag[DeepType3]
      )
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }

  "Cache stress test with types sharing parent structures" should {

    "handle collection types sharing Iterable parent" in {
      val list1 = Tag[List[Int]]; val list2 = Tag[List[Int]]; val list3 = Tag[List[Int]]
      val vec1 = Tag[Vector[Int]]; val vec2 = Tag[Vector[Int]]; val vec3 = Tag[Vector[Int]]
      val set1 = Tag[Set[Int]]; val set2 = Tag[Set[Int]]; val set3 = Tag[Set[Int]]
      
      // Verify repeated materializations produce identical tags
      assertSameStrict(list1.tag, list2.tag)
      assertSameStrict(list1.tag, list3.tag)
      assertSameStrict(vec1.tag, vec2.tag)
      assertSameStrict(vec1.tag, vec3.tag)
      assertSameStrict(set1.tag, set2.tag)
      assertSameStrict(set1.tag, set3.tag)
      
      // Verify all collection types are children of Iterable
      val iterTag = Tag[Iterable[Int]]
      assertChild(list1.tag, iterTag.tag)
      assertChild(vec1.tag, iterTag.tag)
      assertChild(set1.tag, iterTag.tag)
    }

    "handle Option and Either types (both are Product)" in {
      val opt1 = Tag[Option[Int]]; val opt2 = Tag[Option[Int]]; val opt3 = Tag[Option[Int]]
      val opt4 = Tag[Option[Int]]; val opt5 = Tag[Option[Int]]; val opt6 = Tag[Option[Int]]
      val either1 = Tag[Either[Int, String]]; val either2 = Tag[Either[Int, String]]
      val either3 = Tag[Either[Int, String]]; val either4 = Tag[Either[Int, String]]
      
      // Verify all Option materializations produce identical tags
      assertSameStrict(opt1.tag, opt2.tag)
      assertSameStrict(opt1.tag, opt3.tag)
      assertSameStrict(opt1.tag, opt4.tag)
      assertSameStrict(opt1.tag, opt5.tag)
      assertSameStrict(opt1.tag, opt6.tag)
      
      // Verify all Either materializations produce identical tags
      assertSameStrict(either1.tag, either2.tag)
      assertSameStrict(either1.tag, either3.tag)
      assertSameStrict(either1.tag, either4.tag)
    }
  }

  "Cache stress test with higher-kinded types" should {

    "handle TagK repeated materializations" in {
      val tk1 = TagK[List]; val tk2 = TagK[List]; val tk3 = TagK[List]
      val tk4 = TagK[List]; val tk5 = TagK[List]; val tk6 = TagK[List]
      val tk7 = TagK[List]; val tk8 = TagK[List]; val tk9 = TagK[List]
      val tk10 = TagK[List]
      
      // Verify all TagK materializations produce identical tags
      val allTagKs = List(tk1, tk2, tk3, tk4, tk5, tk6, tk7, tk8, tk9, tk10)
      allTagKs.foreach(tk => assertSameStrict(tk1.tag, tk.tag))
    }

    "handle TagKK repeated materializations" in {
      val tkk1 = TagKK[Either]; val tkk2 = TagKK[Either]; val tkk3 = TagKK[Either]
      val tkk4 = TagKK[Either]; val tkk5 = TagKK[Either]; val tkk6 = TagKK[Either]
      val tkk7 = TagKK[Either]; val tkk8 = TagKK[Either]; val tkk9 = TagKK[Either]
      val tkk10 = TagKK[Either]
      
      // Verify all TagKK materializations produce identical tags
      val allTagKKs = List(tkk1, tkk2, tkk3, tkk4, tkk5, tkk6, tkk7, tkk8, tkk9, tkk10)
      allTagKKs.foreach(tkk => assertSameStrict(tkk1.tag, tkk.tag))
    }
  }
}
