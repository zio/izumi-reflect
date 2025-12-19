package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl

import java.lang.ref.SoftReference
import java.util.concurrent.ConcurrentHashMap
import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  private case class CacheEntry(ltt: LightTypeTag, structuralKey: String)
  private val lttCache = new ConcurrentHashMap[String, SoftReference[CacheEntry]]()
  private val cacheHits = new java.util.concurrent.atomic.AtomicLong(0)
  private val cacheMisses = new java.util.concurrent.atomic.AtomicLong(0)

  def getCacheStats: (Long, Long, Int) = (cacheHits.get(), cacheMisses.get(), lttCache.size())
  def resetCacheStats(): Unit = { cacheHits.set(0); cacheMisses.set(0) }

  private def lttCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    inspectTypeRepr(qctx.reflect.TypeRepr.of[T])
  }

  def inspectTypeRepr(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Expr[LightTypeTag] = {
    import qctx.reflect.*

    val structuralKey = makeStructuralKey(typeRepr)

    val cachedLtt: Option[LightTypeTag] =
      if (lttCacheEnabled) {
        Option(lttCache.get(structuralKey))
          .flatMap(sr => Option(sr.get()))
          .filter(_.structuralKey == structuralKey)
          .map(_.ltt)
      } else {
        None
      }

    cachedLtt match {
      case Some(_) => cacheHits.incrementAndGet()
      case None => cacheMisses.incrementAndGet()
    }

    val ltt = cachedLtt.getOrElse {
      val ref = TypeInspections(typeRepr)
      val fullDb = TypeInspections.fullDb(typeRepr)
      val nameDb = TypeInspections.unappliedDb(typeRepr)
      val newLtt = LightTypeTag(ref, fullDb, nameDb)

      if (lttCacheEnabled) {
        lttCache.put(structuralKey, new SoftReference(CacheEntry(newLtt, structuralKey)))
      }

      newLtt
    }

    makeParsedLightTypeTagImpl(ltt)
  }

  private def makeStructuralKey(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): String = {
    import qctx.reflect.*
    val dealiased = typeRepr.dealias.simplified
    val baseTypesKey = dealiased.baseClasses
      .map { sym =>
        val numTypeParams = sym.typeMembers.count(_.isTypeParam)
        val posKey = sym.pos.map(p => s"@${p.sourceFile.path}:${p.start}").getOrElse("")
        s"${sym.fullName}#$numTypeParams$posKey"
      }
      .sorted
      .mkString(";")
    s"${dealiased.show}|bases:$baseTypesKey"
  }

  def inspectStrong[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    val owners = ReflectionUtil.getClassDefOwners(Symbol.spliceOwner)
    if (ReflectionUtil.allPartsStrong(0, owners, Set.empty, tpe)) {
      inspectAny[T]
    } else {
      report.errorAndAbort(s"Can't materialize LTag[$tpe]: found unresolved type parameters in $tpe")
    }
  }

  def makeParsedLightTypeTagImpl(ltt: LightTypeTag)(using qctx: Quotes): Expr[LightTypeTag] = {
    val serialized = ltt.serialize()
    val hashCodeRef = serialized.hash
    val strRef = serialized.ref
    val strDBs = serialized.databases

    InspectorBase.ifDebug {
      def string2hex(str: String): String = str.toList.map(_.toInt.toHexString).mkString

      println(s"${ltt.ref} => ${strRef.size} bytes, ${string2hex(strRef)}")
      println(s"${SubtypeDBs.make(ltt.basesdb, ltt.idb)} => ${strDBs.size} bytes, ${string2hex(strDBs)}")
      println(strDBs)
    }

    '{ LightTypeTag.parse(${ Expr(hashCodeRef) }, ${ Expr(strRef) }, ${ Expr(strDBs) }, ${ Expr(LightTypeTag.currentBinaryFormatVersion) }) }
  }

}
