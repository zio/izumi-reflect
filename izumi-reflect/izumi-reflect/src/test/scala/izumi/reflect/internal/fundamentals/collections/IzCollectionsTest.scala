
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

package izumi.reflect.fundamentals.collections

import izumi.reflect.fundamentals.collections.IzCollections._
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class IzCollectionsTest extends AnyWordSpec {
  "Collection utils" should {
    "allow to convert mappings to multimaps" in {
      assert(List("a" -> 1, "a" -> 2).toMultimap == Map("a" -> Set(1, 2)))
      assert(List("a" -> 1, "a" -> 2).toMutableMultimap == mutable.Map("a" -> mutable.Set(1, 2)))
    }
  }
}


