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

package izumi.reflect.macrortti

import java.lang.ref.SoftReference
import java.util.WeakHashMap

/**
  * A cache with weakly-held keys and softly-held values, intended for compile-time and runtime
  * caching of computed LightTypeTag parts.
  *
  * Values are wrapped into [[SoftReference]]s, so it is always safe to recompute them
  * when the GC decides to collect the cached entries; the cache will never cause
  * a compiler crash due to heap exhaustion.
  */
private[reflect] final class SoftValueCache[K, V <: AnyRef] {

  private[this] val cache = new WeakHashMap[K, SoftReference[V]]()

  def get(key: K): Option[V] = {
    cache.synchronized {
      val ref = cache.get(key)
      if (ref == null) {
        None
      } else {
        Option(ref.get)
      }
    }
  }

  def put(key: K, value: V): Unit = {
    cache.synchronized {
      cache.put(key, new SoftReference(value))
    }
  }

  def getOrElseUpdate(key: K, compute: => V): V = {
    cache.synchronized {
      val ref = cache.get(key)
      val cached = if (ref == null) None else Option(ref.get)
      cached match {
        case Some(value) =>
          value
        case None =>
          val value = compute
          cache.put(key, new SoftReference(value))
          value
      }
    }
  }

}
