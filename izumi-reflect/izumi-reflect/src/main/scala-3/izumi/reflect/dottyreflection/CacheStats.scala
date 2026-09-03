package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties

import java.util.concurrent.atomic.AtomicLong

/**
 * Cache statistics for compile-time LightTypeTag caching.
 * 
 * Enable stats output by setting system property:
 *   -Dizumi.reflect.rtti.cache.compile.stats=true
 */
object CacheStats {
  // Term cache (tree-level)
  val termCacheHits = new AtomicLong(0)
  val termCacheMisses = new AtomicLong(0)

  // LTT cache
  val lttCacheHits = new AtomicLong(0)
  val lttCacheMisses = new AtomicLong(0)

  // Serialized cache
  val serializedCacheHits = new AtomicLong(0)
  val serializedCacheMisses = new AtomicLong(0)

  // FullDB cache
  val fullDbCacheHits = new AtomicLong(0)
  val fullDbCacheMisses = new AtomicLong(0)

  // InheritanceDB cache
  val inheritanceDbCacheHits = new AtomicLong(0)
  val inheritanceDbCacheMisses = new AtomicLong(0)

  private def statsEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.stats`))
      .flatMap(_.asBoolean())
      .getOrElse(false)
  }

  def termHit(): Unit = if (statsEnabled) termCacheHits.incrementAndGet()
  def termMiss(): Unit = if (statsEnabled) termCacheMisses.incrementAndGet()

  def lttHit(): Unit = if (statsEnabled) lttCacheHits.incrementAndGet()
  def lttMiss(): Unit = if (statsEnabled) lttCacheMisses.incrementAndGet()

  def serializedHit(): Unit = if (statsEnabled) serializedCacheHits.incrementAndGet()
  def serializedMiss(): Unit = if (statsEnabled) serializedCacheMisses.incrementAndGet()

  def fullDbHit(): Unit = if (statsEnabled) fullDbCacheHits.incrementAndGet()
  def fullDbMiss(): Unit = if (statsEnabled) fullDbCacheMisses.incrementAndGet()

  def inheritanceDbHit(): Unit = if (statsEnabled) inheritanceDbCacheHits.incrementAndGet()
  def inheritanceDbMiss(): Unit = if (statsEnabled) inheritanceDbCacheMisses.incrementAndGet()

  def printStats(): Unit = {
    if (statsEnabled) {
      val sb = new StringBuilder
      sb.append("\n=== izumi-reflect compile-time cache stats ===\n")
      sb.append(f"termCache:        hits=${termCacheHits.get()}%6d  misses=${termCacheMisses.get()}%6d\n")
      sb.append(f"lttCache:         hits=${lttCacheHits.get()}%6d  misses=${lttCacheMisses.get()}%6d\n")
      sb.append(f"serializedCache:  hits=${serializedCacheHits.get()}%6d  misses=${serializedCacheMisses.get()}%6d\n")
      sb.append(f"fullDbCache:      hits=${fullDbCacheHits.get()}%6d  misses=${fullDbCacheMisses.get()}%6d\n")
      sb.append(f"inheritanceDbCache: hits=${inheritanceDbCacheHits.get()}%6d  misses=${inheritanceDbCacheMisses.get()}%6d\n")
      sb.append("===============================================\n")
      System.err.println(sb.toString())
    }
  }

  // Register shutdown hook to print stats at JVM exit
  if (statsEnabled) {
    Runtime.getRuntime.addShutdownHook(new Thread(() => printStats()))
  }
}
