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

package izumi.fundamentals.platform.cache

import izumi.fundamentals.platform.language.Quirks._

import scala.collection.mutable

class SyncCache[K, V] {
  private[this] val cache = new mutable.HashMap[K, V]()

  def enumerate(): Seq[(K, V)] = synchronize {
    cache.toSeq
  }

  def getOrCompute(k: K, default: => V): V = synchronize {
    cache.getOrElseUpdate(k, default)
  }

  def put(k: K, v: V): Unit = synchronize {
    cache.put(k, v).discard()
  }

  def get(k: K): Option[V] = synchronize {
    cache.get(k)
  }

  def clear(): Unit = synchronize {
    cache.clear()
  }

  def hasKey(k: K): Boolean = synchronize {
    cache.contains(k)
  }

  def putIfNotExist(k: K, v: => V): Unit = synchronize {
    if (!cache.contains(k)) {
      cache.put(k, v).discard()
    }
  }

  def size: Int = synchronize {
    cache.size
  }

  private[this] def synchronize[T](f: => T): T = {
    cache.synchronized {
      f
    }
  }

}


