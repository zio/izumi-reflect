package izumi.reflect.macr

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macr.LightTypeTagMacro.logger
import izumi.reflect.macr.tags.LightTypeTagFactory

import java.lang.ref.SoftReference
import scala.collection.concurrent.TrieMap
import scala.quoted.{Expr, Quotes, Type}

object LightTypeTagMacro {
  private[reflect] final val logger = new TrivialLogger(
    "izumi.reflect.macr.LightTypeTagMacro",
    // For a normal user, this should be disabled.
    // For a developer working on izumi-reflect, this should be enabled.
    // The value is hardcoded because it's only a debug setting.
    debug = false,
  )

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeLightTypeTag[T]: LightTypeTag = ${ makeLightTypeTagImpl[T] }

  def makeLightTypeTagImpl[T: Type](using q: Quotes): Expr[LightTypeTag] = {
    val tpe = Type.of[T]
    val macroContext = MacroContext(q)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeLightTypeTagForType(inline tpe: Any): LightTypeTag = ${ makeLightTypeTagForTypeImpl('tpe) }

  def makeLightTypeTagForTypeImpl(tpe: Expr[Any])(using q: Quotes): Expr[LightTypeTag] = {
    import q.reflect.*
    val typeRepr = tpe.asTerm.tpe.widen
    val macroContext = MacroContext(q)
    macroContext.makeLtt(typeRepr.asType)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeLightTypeTagForTypeParam[T]: LightTypeTag = ${ makeLightTypeTagForTypeParamImpl[T] }

  def makeLightTypeTagForTypeParamImpl[T: Type](using q: Quotes): Expr[LightTypeTag] = {
    val tpe = Type.of[T]
    val macroContext = MacroContext(q)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeParsedLightTypeTag[T]: LightTypeTag = ${ makeParsedLightTypeTagImpl[T] }

  def makeParsedLightTypeTagImpl[T: Type](using q: Quotes): Expr[LightTypeTag] = {
    val tpe = Type.of[T]
    val macroContext = MacroContext(q)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeParsedLightTypeTagForType(inline tpe: Any): LightTypeTag = ${ makeParsedLightTypeTagForTypeImpl('tpe) }

  def makeParsedLightTypeTagForTypeImpl(tpe: Expr[Any])(using q: Quotes): Expr[LightTypeTag] = {
    import q.reflect.*
    val typeRepr = tpe.asTerm.tpe.widen
    val macroContext = MacroContext(q)
    macroContext.makeLtt(typeRepr.asType)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  inline def makeParsedLightTypeTagForTypeParam[T]: LightTypeTag = ${ makeParsedLightTypeTagForTypeParamImpl[T] }

  def makeParsedLightTypeTagForTypeParamImpl[T: Type](using q: Quotes): Expr[LightTypeTag] = {
    val tpe = Type.of[T]
    val macroContext = MacroContext(q)
    macroContext.makeLtt(tpe)
  }

  final case class MacroContext(q: Quotes) {
    import q.reflect.*

    val typeTagFactory: LightTypeTagFactory[q.type] = new LightTypeTagFactory[q.type](q)

    private val lttCache: TrieMap[TypeRepr, SoftReference[Expr[LightTypeTag]]] = TrieMap.empty[TypeRepr, SoftReference[Expr[LightTypeTag]]]

    def makeLtt(tpe: Type[?]): Expr[LightTypeTag] = {
      val typeRepr = tpe.asType
      val key = typeRepr.dealias // Use dealiased type as cache key

      if (IzumiReflectConfig.enableMacroLevelCache) {
        lttCache.get(key).flatMap(ref => Option(ref.get())) match {
          case Some(cached) =>
            logger.debug(s"LightTypeTagMacro: Cache HIT for type $typeRepr (key: $key)")
            cached
          case None =>
            logger.debug(s"LightTypeTagMacro: Cache MISS for type $typeRepr (key: $key)")
            val ltt = typeTagFactory.makeLightTypeTag(this)(tpe)
            lttCache.put(key, new SoftReference(ltt))
            ltt
        }
      } else {
        logger.debug(s"LightTypeTagMacro: Macro-level cache disabled for type $typeRepr")
        typeTagFactory.makeLightTypeTag(this)(tpe)
      }
    }
  }
}
