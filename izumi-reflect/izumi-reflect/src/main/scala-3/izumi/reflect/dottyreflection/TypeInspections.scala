package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import java.lang.ref.SoftReference
import scala.quoted.Quotes

object TypeInspections {
  private type TypeReprKey = Quotes#reflectModule#TypeRepr

  private final class SoftWeakCache[K, V <: AnyRef] {
    private val map = new java.util.WeakHashMap[K, SoftReference[V]]()

    def get(key: K): Option[V] = map.synchronized {
      val ref = map.get(key)
      if (ref eq null) None else Option(ref.get())
    }

    def put(key: K, value: V): V = {
      map.synchronized {
        map.put(key, new SoftReference(value))
      }
      value
    }

    def getOrElseUpdate(enabled: Boolean, key: K)(compute: => V): V = {
      if (!enabled) {
        compute
      } else {
        get(key).getOrElse(put(key, compute))
      }
    }
  }

  private lazy val refCache = new SoftWeakCache[TypeReprKey, AbstractReference]
  private lazy val fullDbCache = new SoftWeakCache[TypeReprKey, Map[AbstractReference, Set[AbstractReference]]]
  private lazy val unappliedDbCache = new SoftWeakCache[TypeReprKey, Map[NameReference, Set[NameReference]]]
  private lazy val serializedCache = new SoftWeakCache[TypeReprKey, LightTypeTag.Serialized]

  private def flagEnabled(prop: String, default: Boolean): Boolean = {
    System.getProperty(prop).asBoolean().getOrElse(default)
  }

  private lazy val compileCacheEnabled: Boolean = flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile`, default = true)
  private lazy val refCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.ref`, default = true)
  private lazy val fullDbCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.db.full`, default = true)
  private lazy val inheritanceDbCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.db.inheritance`, default = true)
  private lazy val serializedCacheEnabled: Boolean = compileCacheEnabled && flagEnabled(DebugProperties.`izumi.reflect.rtti.cache.compile.serialized`, default = true)

  private def normalizeKey(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): TypeReprKey = {
    typeRepr.dealias.simplified
  }

  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    refCache.getOrElseUpdate(refCacheEnabled, normalizeKey(typeRepr)) {
      Inspector.make(qctx).buildTypeRef(typeRepr)
    }
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    unappliedDbCache.getOrElseUpdate(inheritanceDbCacheEnabled, normalizeKey(typeRepr)) {
      InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
    }
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    fullDbCache.getOrElseUpdate(fullDbCacheEnabled, normalizeKey(typeRepr)) {
      FullDbInspector.make(qctx).buildFullDb(typeRepr)
    }
  }

  def serialized(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): LightTypeTag.Serialized = {
    serializedCache.getOrElseUpdate(serializedCacheEnabled, normalizeKey(typeRepr)) {
      val ref = apply(typeRepr)
      val full = fullDb(typeRepr)
      val name = unappliedDb(typeRepr)
      LightTypeTag(ref, full, name).serialize()
    }
  }

}
