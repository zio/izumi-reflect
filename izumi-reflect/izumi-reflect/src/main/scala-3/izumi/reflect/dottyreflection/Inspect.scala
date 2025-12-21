package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs

import java.lang.ref.SoftReference
import java.util.concurrent.ConcurrentHashMap
import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  // LTT cache stores complete LightTypeTag results
  // Uses String key because TypeRepr is path-dependent on Quotes and cannot be stored directly
  private val lttCache = new ConcurrentHashMap[String, SoftReference[LightTypeTag]]()

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
    
    // Use type's symbol-based key for correctness
    // TypeRepr.show can collide for different types with same name
    val cacheKey = if (cacheEnabled) makeTypeKey(typeRepr) else ""

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

    makeParsedLightTypeTagImpl(ltt)
  }

  /**
   * Build an efficient cache key from the type.
   * 
   * Strategy: Use symbol fullName + structural type args for efficiency.
   * This avoids the expensive `show` operation while maintaining correctness.
   * Position info disambiguates local types with the same name.
   * 
   * Note: TypeRepr cannot be used directly as cache key because it's path-dependent
   * on Quotes, which changes between macro invocations. This is a fundamental
   * limitation of Scala 3's macro system for cross-stage safety.
   */
  private def makeTypeKey(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): String = {
    import qctx.reflect.*
    
    val sb = new java.lang.StringBuilder(128)
    
    def appendTypeKey(tpe: TypeRepr): Unit = {
      val dealiased = tpe.dealias.simplified
      dealiased match {
        case AppliedType(tycon, args) =>
          appendTypeKey(tycon)
          sb.append('[')
          var first = true
          args.foreach { arg =>
            if (!first) sb.append(',')
            first = false
            appendTypeKey(arg)
          }
          sb.append(']')
          
        case TypeRef(prefix, name) =>
          prefix match {
            case NoPrefix() => ()
            case _ =>
              appendTypeKey(prefix)
              sb.append('.')
          }
          val sym = dealiased.typeSymbol
          if (sym.isNoSymbol) {
            sb.append(name)
          } else {
            sb.append(sym.fullName)
            if (!sym.flags.is(Flags.Package) && sym.pos.nonEmpty) {
              val p = sym.pos.get
              sb.append('@').append(p.sourceFile.path.hashCode).append(':').append(p.start)
            }
          }
          
        case TermRef(prefix, name) =>
          prefix match {
            case NoPrefix() => ()
            case _ =>
              appendTypeKey(prefix)
              sb.append('.')
          }
          val sym = dealiased.termSymbol
          if (sym.isNoSymbol) {
            sb.append(name)
          } else {
            sb.append(sym.fullName)
          }
          
        case AndType(left, right) =>
          sb.append('(')
          appendTypeKey(left)
          sb.append('&')
          appendTypeKey(right)
          sb.append(')')
          
        case OrType(left, right) =>
          sb.append('(')
          appendTypeKey(left)
          sb.append('|')
          appendTypeKey(right)
          sb.append(')')
          
        case TypeBounds(lo, hi) =>
          sb.append('[')
          appendTypeKey(lo)
          sb.append("..")
          appendTypeKey(hi)
          sb.append(']')
          
        case TypeLambda(paramNames, paramBounds, resultType) =>
          sb.append("λ[")
          paramNames.foreach { n => sb.append(n).append(',') }
          sb.append("=>")
          appendTypeKey(resultType)
          sb.append(']')
          
        case ParamRef(binder, idx) =>
          sb.append("§").append(idx)
          
        case ConstantType(const) =>
          sb.append("const:").append(const.show)
          
        case ThisType(tref) =>
          sb.append("this:")
          appendTypeKey(tref)
          
        case ByNameType(underlying) =>
          sb.append("=>")
          appendTypeKey(underlying)
          
        case AnnotatedType(underlying, _) =>
          appendTypeKey(underlying)
          
        case _ =>
          // Fallback for any other type - use show but this should be rare
          sb.append(dealiased.show)
      }
    }
    
    appendTypeKey(typeRepr)
    sb.toString
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
