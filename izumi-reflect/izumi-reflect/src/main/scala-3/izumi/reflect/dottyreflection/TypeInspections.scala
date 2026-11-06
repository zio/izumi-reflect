package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import java.lang.ref.SoftReference
import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

object TypeInspections {
  private lazy val refCache = new java.util.WeakHashMap[Any, SoftReference[AbstractReference]]
  private lazy val fullDbCache = new java.util.WeakHashMap[Any, SoftReference[Map[AbstractReference, Set[AbstractReference]]]]
  private lazy val unappliedDbCache = new java.util.WeakHashMap[Any, SoftReference[Map[NameReference, Set[NameReference]]]]
  private lazy val serializedCache = new java.util.WeakHashMap[Any, SoftReference[LightTypeTag.Serialized]]

  private def flagEnabled(prop: String, default: Boolean): Boolean = {
    System.getProperty(prop).asBoolean().getOrElse(default)
  }

  private lazy val compileCacheEnabled: Boolean = flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile`, default = true)
  private lazy val refCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.ref`, default = true)
  private lazy val fullDbCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.db.full`, default = true)
  private lazy val inheritanceDbCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.db.inheritance`, default = true)
  private lazy val serializedCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.serialized`, default = true)

  private def getOrCompute[V <: AnyRef](
    enabled: Boolean,
    cache: java.util.WeakHashMap[Any, SoftReference[V]],
    key: Any
  )(compute: => V): V = {
    if (!enabled) {
      compute
    } else {
      val cached = cache.synchronized {
        val ref = cache.get(key)
        if (ref eq null) None else Option(ref.get())
      }
      cached match {
        case Some(value) => value
        case None =>
          val computed = compute
          cache.synchronized {
            cache.put(key, new SoftReference(computed))
          }
          computed
      }
    }
  }

  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    getOrCompute(refCacheEnabled, refCache, typeRepr) {
      Inspector.make(qctx).buildTypeRef(typeRepr)
    }
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    getOrCompute(inheritanceDbCacheEnabled, unappliedDbCache, typeRepr) {
      InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
    }
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    getOrCompute(fullDbCacheEnabled, fullDbCache, typeRepr) {
      FullDbInspector.make(qctx).buildFullDb(typeRepr)
    }
  }

  def serialized(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): LightTypeTag.Serialized = {
    getOrCompute(serializedCacheEnabled, serializedCache, typeRepr) {
      val ref = apply(typeRepr)
      val full = fullDb(typeRepr)
      val name = unappliedDb(typeRepr)
      LightTypeTag(ref, full, name).serialize()
    }
  }

}
