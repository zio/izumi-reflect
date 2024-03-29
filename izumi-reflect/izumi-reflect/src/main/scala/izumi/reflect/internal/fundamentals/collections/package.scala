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

package izumi.reflect.internal.fundamentals

import izumi.reflect.internal.NowarnCompat

import scala.collection.mutable

package object collections {
  @NowarnCompat.nowarn("msg=deprecated")
  private[reflect] type MutableMultiMap[A, B] = mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]
  private[reflect] type ImmutableMultiMap[A, B] = Map[A, Set[B]]
}
