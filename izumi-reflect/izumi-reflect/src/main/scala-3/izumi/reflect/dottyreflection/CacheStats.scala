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

  def fullDbHit(): Unit = if (statsEnabled) fullDbCacheHits.incrementAndGet()
  def fullDbMiss(): Unit = if (statsEnabled) fullDbCacheMisses.incrementAndGet()

  def inheritanceDbHit(): Unit = if (statsEnabled) inheritanceDbCacheHits.incrementAndGet()
  def inheritanceDbMiss(): Unit = if (statsEnabled) inheritanceDbCacheMisses.incrementAndGet()

  def printStats(): Unit = {
    if (statsEnabled) {
      def row(name: String, hits: Long, misses: Long): String = {
        val total = hits + misses
        val rate = if (total == 0) 100.0 else hits.toDouble / total * 100
        f"$name%-20s $hits%8d $misses%8d $rate%5.1f%%\n"
      }
      val sb = new StringBuilder
      sb.append("\n=== izumi-reflect compile-time cache stats ===\n")
      sb.append(f"${"cache"}%-20s ${"hits"}%8s ${"misses"}%8s ${"hit%"}%6s\n")
      sb.append(row("termCache", termCacheHits.get(), termCacheMisses.get()))
      sb.append(row("fullDbCache", fullDbCacheHits.get(), fullDbCacheMisses.get()))
      sb.append(row("inheritanceDbCache", inheritanceDbCacheHits.get(), inheritanceDbCacheMisses.get()))
      sb.append("==============================================\n")
      System.err.println(sb.toString())
    }
  }

  // Register shutdown hook to print stats at JVM exit
  if (statsEnabled) {
    Runtime.getRuntime.addShutdownHook(new Thread(() => printStats()))
  }
}
