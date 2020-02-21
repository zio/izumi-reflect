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

package izumi.fundamentals.graphs

import scala.annotation.tailrec
import scala.collection.immutable

class Toposort {

  import Toposort._

  @tailrec
  final def cycleBreaking[T](toPreds: Map[T, Set[T]], done: Seq[T], break: immutable.Set[T] => T): Either[InconsistentInput[T], Seq[T]] = {
    val (noPreds, hasPreds) = toPreds.partition {
      _._2.isEmpty
    }

    if (noPreds.isEmpty) {
      if (hasPreds.isEmpty) {
        Right(done)
      } else { // circular dependency
        val loopMembers = hasPreds.filterKeys(isInvolvedIntoCycle(hasPreds))
        if (loopMembers.nonEmpty) {
          val breakLoopAt = break(loopMembers.keySet.toSet)
          val found = Set(breakLoopAt)
          val next = hasPreds.filterKeys(k => k != breakLoopAt).mapValues(_ -- found).toMap

          cycleBreaking(next, done ++ found, break)
        } else {
          Left(InconsistentInput(hasPreds))
        }
      }
    } else {
      val found = noPreds.keySet
      val next = hasPreds.mapValues(_ -- found).toMap
      cycleBreaking(next, done ++ found, break)
    }
  }

  private def isInvolvedIntoCycle[T](toPreds: Map[T, Set[T]])(key: T): Boolean = {
    test(toPreds, Set.empty, key, key)
  }

  private def test[T](toPreds: Map[T, Set[T]], stack: Set[T], toTest: T, needle: T): Boolean = {
    val deps = toPreds.getOrElse(toTest, Set.empty)

    if (deps.contains(needle)) {
      true
    } else {
      deps.exists {
        d =>
          if (stack.contains(d)) {
            false
          } else {
            test(toPreds, stack + d, d, needle)
          }
      }
    }
  }

}

object Toposort {

  final case class InconsistentInput[T](issues: Map[T, Set[T]])

}



