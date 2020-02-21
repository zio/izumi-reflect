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

import org.scalatest.wordspec.AnyWordSpec


class WildcardPrefixTreeTest extends AnyWordSpec {
  def call[K, V](tree: WildcardPrefixTree[K, V], path: K*): Set[V] = {
    tree.findSubtrees(path.toList).flatMap(_.subtreeValues).toSet
  }

  "prefix tree" should {
    "support prefix search" in {
      val tree = WildcardPrefixTree.build(Seq(
        (Seq(Some("a"), Some("b"), Some("c")), 1),
        (Seq(Some("a"), Some("b")), 2),
        (Seq(Some("a"), Some("b"), Some("d")), 3)
      ))

      assert(call(tree, "a") == Set(1, 2, 3))
      assert(call(tree, "a", "b") == Set(1, 2, 3))
      assert(call(tree, "a", "b", "c") == Set(1))
      assert(call(tree) == Set(1, 2, 3))

      assert(call(tree, "x", "y", "z").isEmpty)

      assert(call(tree, "a", "b", "c", "d").isEmpty)
      assert(call(tree, "a", "b", "x", "d").isEmpty)
    }

    "support wildcards search" in {
      val tree = WildcardPrefixTree.build(Seq(
        (Seq(Some("a"), None, Some("c")), 1),
        (Seq(Some("a"), None, Some("d")), 3),
        (Seq(Some("a"), Some("b")), 2)
      ))

      assert(call(tree, "a") == Set(1, 2, 3))
      assert(call(tree, "a", "b") == Set(1, 2, 3))
      assert(call(tree, "a", "x") == Set(1, 3))
    }
  }

}
