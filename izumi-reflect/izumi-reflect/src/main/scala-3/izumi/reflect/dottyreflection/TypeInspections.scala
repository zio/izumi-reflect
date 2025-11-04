package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}
import izumi.reflect.internal.cache.{CacheContext, Scala3KeyGen}

import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

object TypeInspections {
  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr, cacheContext: CacheContext = CacheContext.disabled()): AbstractReference = {
    val cacheKey = Scala3KeyGen.stableKey(typeRepr)
    cacheContext.lttCache.getOrCompute(cacheKey) {
      Inspector.make(qctx, cacheContext).buildTypeRef(typeRepr)
    }
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr, cacheContext: CacheContext = CacheContext.disabled()): Map[NameReference, Set[NameReference]] = {
    val cacheKey = s"unapplied:${Scala3KeyGen.stableKey(typeRepr)}"
    cacheContext.dbCache.getOrCompute(cacheKey) {
      InheritanceDbInspector.make(qctx, cacheContext).makeUnappliedInheritanceDb(typeRepr)
    }.asInstanceOf[Map[NameReference, Set[NameReference]]]
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr, cacheContext: CacheContext = CacheContext.disabled()): Map[AbstractReference, Set[AbstractReference]] = {
    val cacheKey = s"fulldb:${Scala3KeyGen.stableKey(typeRepr)}"
    cacheContext.dbCache.getOrCompute(cacheKey) {
      FullDbInspector.make(qctx, cacheContext).buildFullDb(typeRepr)
    }.asInstanceOf[Map[AbstractReference, Set[AbstractReference]]]
  }

}
