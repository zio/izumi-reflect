package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString._
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTagRef.AbstractReference
import izumi.reflect.macrortti.SoftValueCache

import scala.quoted.Quotes

/**
  * Compile-time caches for the Scala 3 implementation.
  *
  * All cached values are wrapped into `SoftReference`s (see [[SoftValueCache]]),
  * so it is always safe to recompute them and the compiler will never crash
  * because of heap exhaustion caused by these caches.
  *
  * Each cache can be disabled individually with a system property, see [[DebugProperties]].
  */
private[dottyreflection] object CompileTimeCaches {

  private val refsCacheEnabled: Boolean = {
    System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.refs`).asBoolean().getOrElse(true)
  }

  private val serializedCacheEnabled: Boolean = {
    System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.serialized`).asBoolean().getOrElse(true)
  }

  private val refsCache = new SoftValueCache[Any, AbstractReference]
  private val serializedCache = new SoftValueCache[Any, LightTypeTag.Serialized]

  def refFor(typeRepr: Any)(compute: => AbstractReference): AbstractReference = {
    if (refsCacheEnabled) {
      refsCache.getOrElseUpdate(typeRepr, compute)
    } else {
      compute
    }
  }

  def serializedFor(typeRepr: Any)(compute: => LightTypeTag.Serialized): LightTypeTag.Serialized = {
    if (serializedCacheEnabled) {
      serializedCache.getOrElseUpdate(typeRepr, compute)
    } else {
      compute
    }
  }

}
