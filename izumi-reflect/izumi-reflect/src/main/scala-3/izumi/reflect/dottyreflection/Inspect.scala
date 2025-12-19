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

    def normalizedPrefixKey(qualifier: TypeRepr, symbol: Symbol, depth: Int): String = {
      qualifier match {
        case _: ThisType | _: SuperType | _: RecursiveThis =>
          val maybeOwner = symbol.maybeOwner
          if (maybeOwner.exists && !maybeOwner.isNoSymbol && !maybeOwner.isPackageDef && !maybeOwner.isDefDef && !maybeOwner.isTypeDef && !maybeOwner.isLocalDummy) {
            maybeOwner.fullName + "::"
          } else {
            ""
          }
        case NoPrefix() => ""
        case other => loop(other, depth + 1) + "::"
      }
    }

    def loop(tpe: TypeRepr, depth: Int): String = {
      if (depth > 100) return tpe.show

      val dealiased = tpe.dealias.simplified
      dealiased match {
        case AppliedType(tycon, args) =>
          val tyconKey = loop(tycon, depth + 1)
          val argsKey = args.map(arg => loop(arg, depth + 1)).mkString(",")
          s"$tyconKey[$argsKey]"

        case AndType(left, right) =>
          s"(${loop(left, depth + 1)}&${loop(right, depth + 1)})"

        case OrType(left, right) =>
          s"(${loop(left, depth + 1)}|${loop(right, depth + 1)})"

        case TypeBounds(lo, hi) =>
          s"[${loop(lo, depth + 1)}..${loop(hi, depth + 1)}]"

        case TypeLambda(paramNames, paramBounds, resType) =>
          val params = paramNames.zip(paramBounds).map { case (n, b) => s"$n:${loop(b, depth + 1)}" }.mkString(",")
          s"λ($params)=>${loop(resType, depth + 1)}"

        case ref @ TypeRef(qualifier, name) =>
          val prefixKey = normalizedPrefixKey(qualifier, ref.typeSymbol, depth)
          s"$prefixKey$name"

        case ref @ TermRef(qualifier, name) =>
          val prefixKey = normalizedPrefixKey(qualifier, ref.termSymbol, depth)
          s"$prefixKey$name.type"

        case ThisType(tref) =>
          loop(tref, depth + 1)

        case Refinement(parent, name, info) =>
          s"(${loop(parent, depth + 1)}{$name:${loop(info, depth + 1)}})"

        case ByNameType(underlying) =>
          s"=>${loop(underlying, depth + 1)}"

        case ConstantType(const) =>
          s"const(${const.show})"

        case ParamRef(binder, idx) =>
          s"$$param$idx"

        case _ =>
          tpe.typeSymbol.fullName
      }
    }

    loop(typeRepr, 0)
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
