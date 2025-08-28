package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

import java.util.concurrent.ConcurrentHashMap
import java.lang.ref.SoftReference

object TypeInspections {
  // Cache for DB maps
  private val fullDbCache =
    new ConcurrentHashMap[Any, SoftReference[Map[AbstractReference, Set[AbstractReference]]]]()

  private val unappliedDbCache =
    new ConcurrentHashMap[Any, SoftReference[Map[NameReference, Set[NameReference]]]]()

  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    Inspector.make(qctx).buildTypeRef(typeRepr)
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    val dbCacheEnabled = sys.props.get("izumi.reflect.cache.db").exists(_.toBoolean)
    val key: Any = typeRepr

    if (dbCacheEnabled) {
      val ref    = unappliedDbCache.get(key)
      val cached = if (ref != null) ref.get() else null
      if (cached != null) return cached
    }

    val built = InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)

    if (dbCacheEnabled) {
      unappliedDbCache.put(key, new SoftReference(built))
    }

    built
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    val dbCacheEnabled = sys.props.get("izumi.reflect.cache.db").exists(_.toBoolean)
    val key: Any = typeRepr

    if (dbCacheEnabled) {
      val ref    = fullDbCache.get(key)
      val cached = if (ref != null) ref.get() else null
      if (cached != null) return cached
    }

    val built = FullDbInspector.make(qctx).buildFullDb(typeRepr)

    if (dbCacheEnabled) {
      fullDbCache.put(key, new SoftReference(built))
    }

    built
  }
}