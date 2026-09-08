package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs

import java.lang.ref.SoftReference
import java.util.concurrent.ConcurrentHashMap
import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  private type TypeReprKey = Quotes#reflectModule#TypeRepr
  private type TermValue = Quotes#reflectModule#Term

  // Tree-level cache of generated Terms, values held via SoftReference
  private val termCache = new ConcurrentHashMap[TypeReprKey, SoftReference[TermValue]]()

  // Master switch for all compile-time caching
  private def compileCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  // Term cache flag, see `izumi.reflect.rtti.cache.compile.macro`
  private def termCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.macro`))
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

    val cacheEnabled = compileCacheEnabled && termCacheEnabled

    val cacheKey: TypeReprKey = typeRepr.dealias.simplified

    if (cacheEnabled) {
      val cachedRef = termCache.get(cacheKey)
      val cachedTerm =
        if (cachedRef == null) null
        else cachedRef.get()
      if (cachedTerm != null) {
        CacheStats.termHit()
        return cachedTerm.asInstanceOf[qctx.reflect.Term].asExprOf[LightTypeTag]
      } else {
        if (cachedRef != null) termCache.remove(cacheKey, cachedRef) // reclaimed by GC
        CacheStats.termMiss()
      }
    }

    val ref = TypeInspections(typeRepr)
    val fullDb = TypeInspections.fullDb(typeRepr)
    val nameDb = TypeInspections.unappliedDb(typeRepr)
    val ltt = LightTypeTag(ref, fullDb, nameDb)

    makeParsedLightTypeTagImpl(ltt, cacheKey, cacheEnabled)
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

  private def makeParsedLightTypeTagImpl(ltt: LightTypeTag, cacheKey: TypeReprKey, cacheEnabled: Boolean)(using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*

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

    val resultExpr = '{ LightTypeTag.parse(${ Expr(hashCodeRef) }, ${ Expr(strRef) }, ${ Expr(strDBs) }, ${ Expr(LightTypeTag.currentBinaryFormatVersion) }) }

    if (cacheEnabled) {
      termCache.put(cacheKey, new SoftReference(resultExpr.asTerm.asInstanceOf[TermValue]))
    }

    resultExpr
  }

  def makeParsedLightTypeTagImpl(ltt: LightTypeTag)(using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*

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
