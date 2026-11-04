package izumi.reflect.dottyreflection

import izumi.reflect.dottyreflection.CacheStats.{SoftWeakCache, TypeReprKey}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs

import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  private type TermValue = Quotes#reflectModule#Term

  private val termCache = new SoftWeakCache[TypeReprKey, TermValue]()
  private val lttCache = new SoftWeakCache[TypeReprKey, LightTypeTag]()

  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    inspectTypeRepr(qctx.reflect.TypeRepr.of[T])
  }

  def inspectTypeRepr(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Expr[LightTypeTag] = {
    import qctx.reflect.*

    val termCaching = CacheStats.compileCacheEnabled && CacheStats.macroCacheEnabled
    val lttCaching = CacheStats.compileCacheEnabled && CacheStats.lttCacheEnabled
    val cacheKey: TypeReprKey = typeRepr.dealias.simplified

    if (termCaching) {
      termCache.get(cacheKey) match {
        case Some(cachedTerm) =>
          CacheStats.termHit()
          return cachedTerm.asInstanceOf[qctx.reflect.Term].asExprOf[LightTypeTag]
        case None =>
          CacheStats.termMiss()
      }
    }

    val ltt = if (lttCaching) {
      lttCache.get(cacheKey) match {
        case Some(cachedLtt) =>
          CacheStats.lttHit()
          cachedLtt
        case None =>
          CacheStats.lttMiss()
          val computed = computeLightTypeTag(typeRepr)
          lttCache.put(cacheKey, computed)
          computed
      }
    } else {
      computeLightTypeTag(typeRepr)
    }

    val expr = makeParsedLightTypeTagImpl(ltt)
    if (termCaching) {
      termCache.put(cacheKey, expr.asTerm.asInstanceOf[TermValue])
    }
    expr
  }

  private def computeLightTypeTag(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): LightTypeTag = {
    val ref = TypeInspections(typeRepr)
    val fullDb = TypeInspections.fullDb(typeRepr)
    val nameDb = TypeInspections.unappliedDb(typeRepr)
    LightTypeTag(ref, fullDb, nameDb)
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
