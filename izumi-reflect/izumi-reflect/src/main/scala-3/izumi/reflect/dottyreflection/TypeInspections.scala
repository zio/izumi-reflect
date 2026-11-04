package izumi.reflect.dottyreflection

import izumi.reflect.dottyreflection.CacheStats.{SoftWeakCache, TypeReprKey}
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.Quotes

object TypeInspections {
  private val refCache = new SoftWeakCache[TypeReprKey, AbstractReference]()

  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    val cacheEnabled = CacheStats.compileCacheEnabled && CacheStats.lttCacheEnabled
    if (cacheEnabled) {
      val key: TypeReprKey = typeRepr.dealias.simplified
      refCache.get(key) match {
        case Some(cached) =>
          CacheStats.lttHit()
          cached
        case None =>
          CacheStats.lttMiss()
          val computed = Inspector.make(qctx).buildTypeRef(typeRepr)
          refCache.put(key, computed)
          computed
      }
    } else {
      Inspector.make(qctx).buildTypeRef(typeRepr)
    }
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    FullDbInspector.make(qctx).buildFullDb(typeRepr)
  }

}
