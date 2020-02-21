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

import scala.collection.compat._
import scala.collection.mutable

final class IzIterable[A, Repr[_] <: Iterable[_]](private val xs: Repr[A]) extends AnyVal {

  def distinctBy[B, That](f: A => B)(implicit cbf: BuildFrom[Repr[A], A, That]): That = {
    val builder = cbf.newBuilder(xs)
    val i = xs.iterator.asInstanceOf[Iterator[A]] // 2.13 compat, dirty
    val set = mutable.Set[B]()
    while (i.hasNext) {
      val o = i.next()
      val b = f(o)
      if (!set(b)) {
        set += b
        builder += o
      }
    }
    builder.result()
  }
}
