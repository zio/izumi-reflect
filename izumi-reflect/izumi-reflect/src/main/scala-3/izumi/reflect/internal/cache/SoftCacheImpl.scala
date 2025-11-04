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

import java.lang.ref.SoftReference
import java.util.concurrent.{ConcurrentHashMap, atomic}

/** Concurrent soft-reference cache used in compile-time macros. */
final class SoftCacheImpl[K, V](
  val name: String,
  val enabled: Boolean,
) extends SoftCache[K, V] {

  private val map = new ConcurrentHashMap[K, SoftReference[V]]()
  private val hits = new atomic.AtomicLong(0L)
  private val misses = new atomic.AtomicLong(0L)
  private val evictions = new atomic.AtomicLong(0L)

  private val debugEnabled: Boolean =
    System.getProperty("izumi.reflect.cache.debug", "false").equalsIgnoreCase("true")

  override def getOrCompute(key: K)(compute: => V): V = {
    if (!enabled) return computeWithFallback(compute)

    val ref = map.get(key)
    if (ref != null) {
      val v = ref.get()
      if (v != null) {
        hits.incrementAndGet()
        debugLog(s"hit $key")
        return v
      } else {
        evictions.incrementAndGet()
        map.remove(key, ref)
      }
    }

    misses.incrementAndGet()
    debugLog(s"miss $key")

    val value = computeWithFallback(compute)
    map.put(key, new SoftReference(value))
    value
  }

  override def stats: CacheStats =
    CacheStats(hits.get(), misses.get(), evictions.get(), map.size())

  override def clear(): Unit = {
    map.clear()
    hits.set(0L)
    misses.set(0L)
    evictions.set(0L)
    debugLog("cleared")
  }

  private def computeWithFallback(compute: => V): V =
    try compute
    catch {
      case t: Throwable =>
        System.err.println(s"[cache-$name] computation failed: ${t.getMessage}")
        if (debugEnabled) t.printStackTrace(System.err)
        compute
    }

  private def debugLog(msg: String): Unit =
    if (debugEnabled) System.err.println(s"[cache-$name] $msg ($stats)")

  override def toString: String =
    s"SoftCache[$name](enabled=$enabled, $stats)"
}

object SoftCacheImpl {
  /** Build cache with system property override support. */
  def adaptive[K, V](
    name: String,
    defaultEnabled: Boolean,
    systemProperty: String,
  ): SoftCacheImpl[K, V] = {
    val enabled = sys.props.get(systemProperty) match {
      case Some(v) =>
        v.toLowerCase match {
          case "true" | "1" | "on" | "enabled"      => true
          case "false" | "0" | "off" | "disabled"   => false
          case _ =>
            System.err.println(s"invalid cache flag '$v' for $systemProperty, using default=$defaultEnabled")
            defaultEnabled
        }
      case None => defaultEnabled
    }
    new SoftCacheImpl[K, V](name, enabled)
  }
}