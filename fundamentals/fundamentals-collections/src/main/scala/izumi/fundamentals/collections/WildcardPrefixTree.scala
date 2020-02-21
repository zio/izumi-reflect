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

import izumi.fundamentals.collections
import izumi.fundamentals.collections.WildcardPrefixTree.PathElement


case class WildcardPrefixTree[K, V](values: Seq[V], children: Map[PathElement[K], WildcardPrefixTree[K, V]]) {
  def findSubtrees(prefix: List[K]): Seq[WildcardPrefixTree[K, V]] = {
    prefix match {
      case Nil =>
        Seq(this)
      case head :: tail =>
        val exact = children.get(PathElement.Value(head))
        val wildcard = children.get(PathElement.Wildcard)
        (exact.toSeq ++ wildcard.toSeq).flatMap(_.findSubtrees(tail))
    }
  }

  def subtreeValues: Seq[V] = {
    values ++ children.values.flatMap(_.subtreeValues)
  }
}

object WildcardPrefixTree {
  sealed trait PathElement[+V]
  object PathElement {
    case class Value[V](value: V) extends PathElement[V]
    case object Wildcard extends PathElement[Nothing]
  }

  def build[P, V](pairs: Seq[(Seq[Option[P]], V)]): WildcardPrefixTree[P, V] = {
    val (currentValues, subValues) = pairs.partition(_._1.isEmpty)

    val next = subValues
      .collect {
        case (k :: tail, v) =>
          (k, (tail, v))
      }
      .groupBy(_._1)
      .toSeq
      .map {
        case (k, group) =>
          val wk: PathElement[P] = k match {
            case Some(value) =>
              PathElement.Value(value)
            case None =>
              PathElement.Wildcard
          }
          wk -> build(group.map(_._2))
      }
      .toMap

    collections.WildcardPrefixTree(currentValues.map(_._2), next)
  }
}
