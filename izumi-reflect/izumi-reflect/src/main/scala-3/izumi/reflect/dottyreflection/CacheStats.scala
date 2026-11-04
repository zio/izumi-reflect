package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString

import java.lang.ref.SoftReference
import java.util.concurrent.atomic.AtomicLong
import scala.quoted.Quotes

/**
  * Cache utilities and statistics for compile-time LightTypeTag caching on Scala 3.
  *
  * Enable stats output by setting system property:
  *   -Dizumi.reflect.rtti.cache.compile.stats=true
  */
private[dottyreflection] object CacheStats {
  type TypeReprKey = Quotes#reflectModule#TypeRepr

  final class SoftWeakCache[K, V] {
    private val map = new java.util.WeakHashMap[K, SoftReference[V]]()

    def get(key: K): Option[V] = map.synchronized {
      val ref = map.get(key)
      if (ref eq null) {
        None
      } else {
        val value = ref.get()
        if (value == null) {
          map.remove(key)
          None
        } else {
          Some(value)
        }
      }
    }

    def put(key: K, value: V): Unit = map.synchronized {
      map.put(key, new SoftReference(value))
      ()
    }
  }

  private def boolProp(name: String, default: Boolean): Boolean = {
    Option(System.getProperty(name))
      .flatMap(_.asBoolean())
      .getOrElse(default)
  }

  lazy val compileCacheEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile`, default = true)
  lazy val macroCacheEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile.macro`, default = true)
  lazy val lttCacheEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile.ltt`, default = true)
  lazy val fullDbCacheEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile.db.full`, default = true)
  lazy val inheritanceDbCacheEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile.db.inheritance`, default = true)
  lazy val statsEnabled: Boolean = boolProp(DebugProperties.`izumi.reflect.rtti.cache.compile.stats`, default = false)

  val termCacheHits = new AtomicLong(0)
  val termCacheMisses = new AtomicLong(0)

  val lttCacheHits = new AtomicLong(0)
  val lttCacheMisses = new AtomicLong(0)

  val fullDbCacheHits = new AtomicLong(0)
  val fullDbCacheMisses = new AtomicLong(0)

  val inheritanceDbCacheHits = new AtomicLong(0)
  val inheritanceDbCacheMisses = new AtomicLong(0)

  def termHit(): Unit = if (statsEnabled) { termCacheHits.incrementAndGet(); () }
  def termMiss(): Unit = if (statsEnabled) { termCacheMisses.incrementAndGet(); () }

  def lttHit(): Unit = if (statsEnabled) { lttCacheHits.incrementAndGet(); () }
  def lttMiss(): Unit = if (statsEnabled) { lttCacheMisses.incrementAndGet(); () }

  def fullDbHit(): Unit = if (statsEnabled) { fullDbCacheHits.incrementAndGet(); () }
  def fullDbMiss(): Unit = if (statsEnabled) { fullDbCacheMisses.incrementAndGet(); () }

  def inheritanceDbHit(): Unit = if (statsEnabled) { inheritanceDbCacheHits.incrementAndGet(); () }
  def inheritanceDbMiss(): Unit = if (statsEnabled) { inheritanceDbCacheMisses.incrementAndGet(); () }

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
      sb.append(row("lttCache", lttCacheHits.get(), lttCacheMisses.get()))
      sb.append(row("fullDbCache", fullDbCacheHits.get(), fullDbCacheMisses.get()))
      sb.append(row("inheritanceDbCache", inheritanceDbCacheHits.get(), inheritanceDbCacheMisses.get()))
      sb.append("==============================================\n")
      System.err.println(sb.toString())
    }
  }

  if (statsEnabled) {
    Runtime.getRuntime.addShutdownHook(new Thread(() => printStats()))
  }
}
