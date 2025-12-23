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
  
  // Tree-level cache: stores the generated Term for reuse across macro invocations
  private val termCache = new ConcurrentHashMap[TypeReprKey, TermValue]()
  
  // Value caches (fallback)
  private val lttCache = new ConcurrentHashMap[TypeReprKey, SoftReference[LightTypeTag]]()
  private val serializedCache = new java.util.IdentityHashMap[LightTypeTag, LightTypeTag.Serialized]()

  // Master switch for all compile-time caching
  private def compileCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  // Individual LTT cache flag
  private def lttCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.ltt`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  // Serialized form cache flag (controlled by macro cache flag)
  private def serializedCacheEnabled: Boolean = {
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

    val cacheEnabled = compileCacheEnabled && lttCacheEnabled
    
    val cacheKey: TypeReprKey = typeRepr.dealias.simplified

    if (cacheEnabled) {
      val cachedTerm = termCache.get(cacheKey)
      if (cachedTerm != null) {
        return cachedTerm.asInstanceOf[qctx.reflect.Term].asExprOf[LightTypeTag]
      }
    }

    val cachedLtt: Option[LightTypeTag] =
      if (cacheEnabled) {
        Option(lttCache.get(cacheKey)).flatMap(sr => Option(sr.get()))
      } else {
        None
      }

    val ltt = cachedLtt.getOrElse {
      val ref = TypeInspections(typeRepr)
      val fullDb = TypeInspections.fullDb(typeRepr)
      val nameDb = TypeInspections.unappliedDb(typeRepr)
      val newLtt = LightTypeTag(ref, fullDb, nameDb)

      if (cacheEnabled) {
        lttCache.put(cacheKey, new SoftReference(newLtt))
      }

      newLtt
    }

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
    
    val serCacheEnabled = compileCacheEnabled && serializedCacheEnabled
    
    // Try to get cached serialized form (synchronized because IdentityHashMap is not thread-safe)
    val serialized = if (serCacheEnabled) {
      serializedCache.synchronized {
        val cached = serializedCache.get(ltt)
        if (cached != null) {
          cached
        } else {
          val ser = ltt.serialize()
          serializedCache.put(ltt, ser)
          ser
        }
      }
    } else {
      ltt.serialize()
    }
    
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
      termCache.put(cacheKey, resultExpr.asTerm.asInstanceOf[TermValue])
    }

    resultExpr
  }

  def makeParsedLightTypeTagImpl(ltt: LightTypeTag)(using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    
    val serCacheEnabled = compileCacheEnabled && serializedCacheEnabled
    
    // Try to get cached serialized form (synchronized because WeakHashMap is not thread-safe)
    val serialized = if (serCacheEnabled) {
      serializedCache.synchronized {
        val cached = serializedCache.get(ltt)
        if (cached != null) {
          cached
        } else {
          val ser = ltt.serialize()
          serializedCache.put(ltt, ser)
          ser
        }
      }
    } else {
      ltt.serialize()
    }
    
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
