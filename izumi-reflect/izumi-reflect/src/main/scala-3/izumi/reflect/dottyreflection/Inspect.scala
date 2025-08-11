package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString._

import scala.quoted.{Expr, Quotes, Type}
import java.util.concurrent.ConcurrentHashMap

object Inspect {
  private val compilationCache = new ConcurrentHashMap[Any, LightTypeTag]()
  
  /** caching is enabled by default for compile-time light type tag creation */
  private lazy val compileCacheEnabled: Boolean = {
    System
      .getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`).asBoolean()
      .getOrElse(true)
  }
  
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    
    if (compileCacheEnabled) {
      val tpe = TypeRepr.of[T]
      
      val cached = compilationCache.get(tpe) match {
        case null =>
          val ref = TypeInspections.apply[T]
          val fullDb = TypeInspections.fullDb[T]
          val nameDb = TypeInspections.unappliedDb[T]
          val ltt = LightTypeTag(ref, fullDb, nameDb)
          compilationCache.putIfAbsent(tpe, ltt)
          compilationCache.get(tpe) // Return the cached value in case another thread computed it first
        case cached =>
          cached
      }
      
      makeParsedLightTypeTagImpl(cached)
    } else {
      computeLightTypeTag[T]
    }
  }
  
  private def computeLightTypeTag[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    val ref = TypeInspections.apply[T]
    val fullDb = TypeInspections.fullDb[T]
    val nameDb = TypeInspections.unappliedDb[T]
    val ltt = LightTypeTag(ref, fullDb, nameDb)
    makeParsedLightTypeTagImpl(ltt)
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
