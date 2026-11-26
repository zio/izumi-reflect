package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import java.lang.ref.SoftReference
import scala.collection.mutable
import scala.quoted.Quotes

object TypeInspections {
  private val refCache = mutable.HashMap.empty[String, SoftReference[AbstractReference]]
  private val fullDbCache = mutable.HashMap.empty[String, SoftReference[Map[AbstractReference, Set[AbstractReference]]]]
  private val unappliedDbCache = mutable.HashMap.empty[String, SoftReference[Map[NameReference, Set[NameReference]]]]

  private def cacheEnabled: Boolean = {
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .map(_.trim.toLowerCase)
      .collect {
        case "true" => true
        case "false" => false
      }
      .getOrElse(true)
  }

  private def cacheKey(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): String = {
    import qctx.reflect.*
    s"${typeRepr.hashCode()}::${typeRepr.show(using Printer.TypeReprStructure)}"
  }

  private def cached[A <: AnyRef](cache: mutable.HashMap[String, SoftReference[A]])(key: String)(compute: => A): A = {
    cache.synchronized {
      cache.get(key).flatMap(ref => Option(ref.get)).getOrElse {
        val computed = compute
        cache.put(key, new SoftReference[A](computed))
        computed
      }
    }
  }

  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    if (cacheEnabled) {
      cached(refCache)(cacheKey(typeRepr)) {
        Inspector.make(qctx).buildTypeRef(typeRepr)
      }
    } else {
      Inspector.make(qctx).buildTypeRef(typeRepr)
    }
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    if (cacheEnabled) {
      cached(unappliedDbCache)(cacheKey(typeRepr)) {
        InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
      }
    } else {
      InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
    }
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    if (cacheEnabled) {
      cached(fullDbCache)(cacheKey(typeRepr)) {
        FullDbInspector.make(qctx).buildFullDb(typeRepr)
      }
    } else {
      FullDbInspector.make(qctx).buildFullDb(typeRepr)
    }
  }

}
