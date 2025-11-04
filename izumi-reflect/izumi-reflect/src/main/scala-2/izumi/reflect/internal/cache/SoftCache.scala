/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 */

package izumi.reflect.internal.cache

/** Cache performance statistics. */
final case class CacheStats(
  hits: Long,
  misses: Long,
  evictions: Long,
  size: Int,
) {
  def hitRate: Double =
    if (hits + misses == 0) 0.0 else hits.toDouble / (hits + misses)

  override def toString: String =
    f"hits: $hits, misses: $misses, evictions: $evictions, size: $size, hit-rate: ${hitRate * 100}%.1f%%"
}

object CacheStats {
  val empty: CacheStats = CacheStats(0L, 0L, 0L, 0)
}

/** Thread-safe soft-reference cache with basic statistics. */
trait SoftCache[K, V] {
  /** Return cached value or compute and store it. */
  def getOrCompute(key: K)(compute: => V): V

  /** Current statistics. */
  def stats: CacheStats

  /** Remove all entries. */
  def clear(): Unit

  /** Cache name. */
  def name: String

  /** Whether caching is enabled. */
  def enabled: Boolean
}