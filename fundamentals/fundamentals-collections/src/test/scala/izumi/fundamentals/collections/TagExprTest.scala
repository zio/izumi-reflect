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

package izumi.fundamentals.collections

import izumi.fundamentals.tags.TagExpr
import org.scalatest.wordspec.AnyWordSpec


class TagExprTest extends AnyWordSpec {

  "Tag expression evaluator" should {
    "support pretty-printing" in {
      val xorExpr = TagExpr.Strings.one("a", "b")
      val notXorExpr = TagExpr.Strings.Not(xorExpr)

      val abExpr = TagExpr.Strings.And(Set(
        notXorExpr,
        TagExpr.Strings.all("a", "b")
      ))

      val expr = TagExpr.Strings.And(Set(
        notXorExpr,
        TagExpr.Strings.all("a", "b"),
        TagExpr.Strings.any("x", "y")
      ))
      assert(expr.toString == "(!(:a \\/ :b) && (:a && :b) && (:x || :y))")

      assert(TagExpr.Strings.TagDNF.toDNF(xorExpr).toString == "((!:a && :b) || (!:b && :a))")
      assert(TagExpr.Strings.TagDNF.toDNF(notXorExpr).toString == "((!:a && !:b) || (:a && :b))")
      assert(TagExpr.Strings.TagDNF.toDNF(abExpr).toString == "(:a && :b)")
      assert(TagExpr.Strings.TagDNF.toDNF(expr).toString == "((:a && :b && :x) || (:a && :b && :y))")
    }

    "support boolean operations" in {
      assert(TagExpr.Strings.any("a", "b").evaluate(Set("a")))
      assert(TagExpr.Strings.any("a", "b").evaluate(Set("a", "b")))
      assert(TagExpr.Strings.any("a", "b").evaluate(Set("a", "c")))
      assert(TagExpr.Strings.any("a", "b").evaluate(Set("a", "b", "c")))
      assert(!TagExpr.Strings.any("a", "b").evaluate(Set("d")))

      assert(!TagExpr.Strings.all("a", "b").evaluate(Set("a")))
      assert(TagExpr.Strings.all("a", "b").evaluate(Set("a", "b")))
      assert(!TagExpr.Strings.all("a", "b").evaluate(Set("a", "c")))
      assert(TagExpr.Strings.all("a", "b").evaluate(Set("a", "b", "c")))
      assert(!TagExpr.Strings.all("a", "b").evaluate(Set("d")))


      assert(TagExpr.Strings.one("a", "b").evaluate(Set("a")))
      assert(!TagExpr.Strings.one("a", "b").evaluate(Set("a", "b")))
      assert(TagExpr.Strings.one("a", "b").evaluate(Set("a", "c")))
      assert(!TagExpr.Strings.one("a", "b").evaluate(Set("a", "b", "c")))
      assert(!TagExpr.Strings.one("a", "b").evaluate(Set("d")))
      assert(TagExpr.Strings.Not(TagExpr.Strings.one("a", "b")).evaluate(Set("d")))
    }

    "support expr dsl" in {
      import TagExpr.Strings._
      assert((False || True).evaluate(Set.empty))
      assert(!(False && True).evaluate(Set.empty))
      assert((False ^^ True).evaluate(Set.empty))
      assert((!False).evaluate(Set.empty))
      assert(!(!True).evaluate(Set.empty))
      assert((t"a" && t"b").evaluate(Set("a", "b")))
    }
  }


}
