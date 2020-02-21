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

trait Loops {
  def findCyclesFor[T](node: T, graph: T => Option[Set[T]]): Seq[Seq[T]] = {
    val loops = new scala.collection.mutable.HashSet[Seq[T]]()
    traceCycles(graph, loops)(node, Seq.empty, Set.empty)
    loops.toSeq
  }

  private def traceCycles[T](graph: T => Option[Set[T]], loops: scala.collection.mutable.HashSet[Seq[T]])(current: T, path: Seq[T], seen: Set[T]): Unit = {
    if (seen.contains(current)) {
      loops.add(path :+ current)
      ()
    } else {
      graph(current) match {
        case Some(value) =>
          value.foreach {
            referenced =>
              traceCycles(graph, loops)(referenced, path :+ current, seen + current)
          }
        case None =>
      }
    }
  }

}

object Loops extends Loops {

}
