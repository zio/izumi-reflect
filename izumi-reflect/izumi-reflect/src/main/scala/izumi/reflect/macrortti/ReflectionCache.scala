/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
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

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import java.util.WeakHashMap

/**
  * Granular, per-entity caches for reflection results.
  *
  * Previously a single monolithic `globalCache: WeakHashMap[Any, AbstractReference]` was used inside
  * `LightTypeTagImpl` (Scala 2 only). That design had two problems:
  *
  *   1. It cached exactly one kind of entity (a type `Reference`). Full `LightTypeTag`s and the
  *      per-name inheritance databases were recomputed on every macro invocation, even when the
  *      reference portion was already cached.
  *   2. The Scala 3 macro had no cache at all.
  *
  * This object splits the cache into independent, per-entity weak caches so that each kind of
  * reflection artifact can be cached (and, if needed, invalidated or tuned) separately:
  *
  *   - [[referenceCache]]    : `Type`/`TypeRepr` -> `AbstractReference`
  *   - [[nameReferenceCache]]: `Type`/`TypeRepr` -> `NameReference`
  *   - [[fullTagCache]]      : `Type`/`TypeRepr` -> `LightTypeTag`
  *
  * Keys are held weakly so that compiler / runtime classloaders and `Type`/`TypeRepr` instances can
  * still be garbage-collected; the caches never leak the universe or the quoted context.
  *
  * The caches are shared between the Scala 2 and Scala 3 macros because the cached values
  * (`AbstractReference`, `NameReference`, `LightTypeTag`) are plain structural data defined in the
  * shared `izumi.reflect.macrortti` package.
  */
object ReflectionCache {

  /**
    * A single-entity weak cache. The compute step is deliberately run *outside* the lock (matching
    * the original `globalCache` behaviour) so that the recursive, potentially expensive reflection
    * computation never holds the monitor; `synchronized` is reentrant on the same thread, so the
    * nested cache accesses performed during reflection remain safe.
    */
  private final class EntityCache[V <: AnyRef] {

    private val map = new WeakHashMap[Any, V]

    /** Returns the cached value for `key`, or `null` if absent. */
    def get(key: Any): V = map.synchronized(map.get(key))

    /** Inserts `value` for `key`. */
    def put(key: Any, value: V): Unit = map.synchronized(map.put(key, value))

    /**
      * Returns the cached value for `key`, computing and caching it on a miss.
      *
      * The compute function is evaluated outside the lock to preserve the original non-reentrant
      * locking discipline of the monolithic cache.
      */
    def getOrElseUpdate(key: Any, compute: => V): V = {
      val existing = get(key)
      if (existing != null) existing
      else {
        val value = compute
        put(key, value)
        value
      }
    }
  }

  private val referenceCache = new EntityCache[AbstractReference]
  private val nameReferenceCache = new EntityCache[NameReference]
  private val fullTagCache = new EntityCache[LightTypeTag]

  /**
    * Whether compile-time caching is enabled. Mirrors the Scala 2 `-Xmacro-settings` flag
    * `izumi.reflect.rtti.cache.compile=false`, but read as a system property so that it is also
    * honoured by the Scala 3 macro (which has no portable access to scalac macro settings).
    *
    * Default: `true`.
    */
  def compileCacheEnabled: Boolean = {
    val p = System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`)
    if (p == null) true
    else !p.equalsIgnoreCase("false")
  }

  /** Look up a cached type `Reference`, or `null` on a miss. */
  def getReference(key: Any): AbstractReference = referenceCache.get(key)

  /** Cache a type `Reference`. */
  def putReference(key: Any, value: AbstractReference): Unit = referenceCache.put(key, value)

  /** Look up a cached type `Reference`, computing and caching it on a miss. */
  def getOrElseUpdateReference(key: Any)(compute: => AbstractReference): AbstractReference =
    referenceCache.getOrElseUpdate(key, compute)

  /** Look up a cached `NameReference`, or `null` on a miss. */
  def getNameReference(key: Any): NameReference = nameReferenceCache.get(key)

  /** Cache a `NameReference`. */
  def putNameReference(key: Any, value: NameReference): Unit = nameReferenceCache.put(key, value)

  /** Look up a cached `NameReference`, computing and caching it on a miss. */
  def getOrElseUpdateNameReference(key: Any)(compute: => NameReference): NameReference =
    nameReferenceCache.getOrElseUpdate(key, compute)

  /** Look up a cached full `LightTypeTag`, or `null` on a miss. */
  def getFullTag(key: Any): LightTypeTag = fullTagCache.get(key)

  /** Cache a full `LightTypeTag`. */
  def putFullTag(key: Any, value: LightTypeTag): Unit = fullTagCache.put(key, value)

  /** Look up a cached full `LightTypeTag`, computing and caching it on a miss. */
  def getOrElseUpdateFullTag(key: Any)(compute: => LightTypeTag): LightTypeTag =
    fullTagCache.getOrElseUpdate(key, compute)
}