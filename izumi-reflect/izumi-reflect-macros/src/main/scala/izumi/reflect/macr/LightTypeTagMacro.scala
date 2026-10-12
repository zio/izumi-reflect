package izumi.reflect.macr

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.language.IzumiReflectMacroContext
import izumi.reflect.macr.LightTypeTagMacro.MacroContext
import izumi.reflect.macr.LightTypeTagMacro.MacroContext.u.Type
import izumi.reflect.macr.tags.LightTypeTagFactory

import java.lang.ref.SoftReference
import scala.collection.concurrent.TrieMap
import scala.language.experimental.macros
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox

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
  def makeLightTypeTag[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[LightTypeTag] = {
    import c.universe._
    val macroContext = MacroContext(c)
    val tpe = c.weakTypeOf[T]
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeLightTypeTagForType(c: blackbox.Context)(tpe: c.Type): c.Expr[LightTypeTag] = {
    val macroContext = MacroContext(c)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeLightTypeTagForTypeParam(c: blackbox.Context)(tpe: c.Type): c.Expr[LightTypeTag] = {
    val macroContext = MacroContext(c)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeLightTypeTagForTypeParamImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[LightTypeTag] = {
    import c.universe._
    val macroContext = MacroContext(c)
    val tpe = c.weakTypeOf[T]
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeParsedLightTypeTag[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[LightTypeTag] = {
    import c.universe._
    val macroContext = MacroContext(c)
    val tpe = c.weakTypeOf[T]
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeParsedLightTypeTagForType(c: blackbox.Context)(tpe: c.Type): c.Expr[LightTypeTag] = {
    val macroContext = MacroContext(c)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeParsedLightTypeTagForTypeParam(c: blackbox.Context)(tpe: c.Type): c.Expr[LightTypeTag] = {
    val macroContext = MacroContext(c)
    macroContext.makeLtt(tpe)
  }

  /**
    * LightTypeTag creation macro.
    *
    * @see [[izumi.reflect.macr.LightTypeTag]]
    */
  def makeParsedLightTypeTagForTypeParamImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[LightTypeTag] = {
    import c.universe._
    val macroContext = MacroContext(c)
    val tpe = c.weakTypeOf[T]
    macroContext.makeLtt(tpe)
  }

  final case class MacroContext(c: blackbox.Context) extends IzumiReflectMacroContext {
    override val u: c.universe.type = c.universe
    override val mirror: u.Mirror = c.mirror
    override val typeTagFactory: LightTypeTagFactory[u.type] = new LightTypeTagFactory[u.type](u)

    import u._

    private val lttCache: TrieMap[Type, SoftReference[Expr[LightTypeTag]]] = TrieMap.empty[Type, SoftReference[Expr[LightTypeTag]]]

    def makeLtt(tpe: Type): c.Expr[LightTypeTag] = {
      val key = tpe.dealias // Use dealiased type as cache key
      if (IzumiReflectConfig.enableMacroLevelCache) {
        lttCache.get(key).flatMap(ref => Option(ref.get())) match {
          case Some(cached) =>
            logger.debug(s"LightTypeTagMacro: Cache HIT for type $tpe (key: $key)")
            cached
          case None =>
            logger.debug(s"LightTypeTagMacro: Cache MISS for type $tpe (key: $key)")
            val ltt = typeTagFactory.makeLightTypeTag(this)(tpe)
            val expr = c.Expr[LightTypeTag](ltt)
            lttCache.put(key, new SoftReference(expr))
            expr
        }
      } else {
        logger.debug(s"LightTypeTagMacro: Macro-level cache disabled for type $tpe")
        val ltt = typeTagFactory.makeLightTypeTag(this)(tpe)
        c.Expr[LightTypeTag](ltt)
      }
    }
  }

}
