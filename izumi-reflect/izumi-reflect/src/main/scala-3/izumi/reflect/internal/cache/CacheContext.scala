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

import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef}
import izumi.reflect.DebugProperties

/** Container for all cache tiers used during compile-time reflection. */
final case class CacheContext(
  macroCache: SoftCache[String, Any],
  lttCache: SoftCache[String, LightTypeTagRef.AbstractReference],
  dbCache: SoftCache[String, Any],
) {
  /** Combined statistics for all tiers. */
  def allStats: Map[String, CacheStats] = Map(
    "macro" -> macroCache.stats,
    "ltt"   -> lttCache.stats,
    "db"    -> dbCache.stats,
  )

  /** Clear all caches. */
  def clearAll(): Unit = {
    macroCache.clear()
    lttCache.clear()
    dbCache.clear()
  }

  /** Print cache statistics to stderr. */
  def printStats(): Unit =
    allStats.foreach { case (tier, stats) =>
      System.err.println(s"[cache-$tier] $stats")
    }
}

object CacheContext {
  /** Build a context with adaptive defaults based on system properties and hardware. */
  def adaptive(): CacheContext = {
    val policy = determinePolicy()

    val macroCache = SoftCacheImpl.adaptive[String, Any](
      "macro",
      policy.macroEnabled,
      DebugProperties.`izumi.reflect.rtti.cache.compile.macro`,
    )

    val lttCache = SoftCacheImpl.adaptive[String, LightTypeTagRef.AbstractReference](
      "ltt",
      policy.lttEnabled,
      DebugProperties.`izumi.reflect.rtti.cache.compile.ltt`,
    )

    val dbCache = SoftCacheImpl.adaptive[String, Any](
      "db",
      policy.dbEnabled,
      DebugProperties.`izumi.reflect.rtti.cache.compile.db`,
    )

    CacheContext(macroCache, lttCache, dbCache)
  }

  /** Create a disabled context for testing. */
  def disabled(): CacheContext = CacheContext(
    new SoftCacheImpl[String, Any]("macro-disabled", enabled = false),
    new SoftCacheImpl[String, LightTypeTagRef.AbstractReference]("ltt-disabled", enabled = false),
    new SoftCacheImpl[String, Any]("db-disabled", enabled = false),
  )

  private case class CachePolicy(macroEnabled: Boolean, lttEnabled: Boolean, dbEnabled: Boolean)

  private def determinePolicy(): CachePolicy =
    sys.props.get("izumi.reflect.cache.policy") match {
      case Some(p) => parsePolicyString(p)
      case None    => adaptiveDefaults()
    }

  private def parsePolicyString(policy: String): CachePolicy = {
    val tiers = policy.toLowerCase.split(',').map(_.trim).toSet
    CachePolicy(
      tiers.contains("macro"),
      tiers.contains("ltt"),
      tiers.contains("db"),
    )
  }

  private def adaptiveDefaults(): CachePolicy = {
    val cores = Runtime.getRuntime.availableProcessors()
    if (cores >= 8) CachePolicy(true, true, true)
    else if (cores >= 4) CachePolicy(true, true, false)
    else CachePolicy(true, false, false)
  }
}