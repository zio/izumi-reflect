package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString._

import scala.quoted.{Expr, Quotes, Type}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

object Inspect {
  
  /** caching is enabled by default for compile-time light type tag creation */
  private[this] lazy val compileCacheEnabled: Boolean = {
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .map(_.toLowerCase != "false")
      .getOrElse(true)
  }
  
  private val compilationCache: concurrent.Map[String, (Expr[LightTypeTag], Int)] = 
    new ConcurrentHashMap[String, (Expr[LightTypeTag], Int)]().asScala
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    
    if (compileCacheEnabled) {
      val tpe = TypeRepr.of[T]
      val stableTypeKey = tpe.dealias.show(using Printer.TypeReprStructure)
      
      Inspect.compilationCache.get(stableTypeKey) match {
        case Some(cached) if cached._2 == qctx.hashCode() =>
          cached._1
        case _ =>
          val result = computeLightTypeTag[T]
          Inspect.compilationCache.put(stableTypeKey, (result, qctx.hashCode()))
          result
      }
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