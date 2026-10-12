package izumi.reflect.macr.tags

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.language.IzumiReflectMacroContext
import izumi.reflect.macr.IzumiReflectConfig
import izumi.reflect.macr.LightTypeTag.Parsed
import izumi.reflect.macr.tags.LightTypeTagFactory.logger
import izumi.reflect.macr.{LightTypeTag, LightTypeTagMacro}

import java.lang.ref.SoftReference
import scala.collection.concurrent.TrieMap
import scala.reflect.api.Universe

class LightTypeTagFactory[U <: Universe](val u: U) {
  import u._

  private val lttCache: TrieMap[Type, SoftReference[LightTypeTag]] = TrieMap.empty[Type, SoftReference[LightTypeTag]]
  private val basesDbCache: TrieMap[Type, SoftReference[Map[Type, Set[Type]]]] = TrieMap.empty[Type, SoftReference[Map[Type, Set[Type]]]]
  private val inheritanceDbCache: TrieMap[Type, SoftReference[Map[Type, Set[Type]]]] = TrieMap.empty[Type, SoftReference[Map[Type, Set[Type]]]]
  private val serializedLttCache: TrieMap[Type, SoftReference[String]] = TrieMap.empty[Type, SoftReference[String]]

  def makeLightTypeTag(c: IzumiReflectMacroContext)(tpe: Type): Tree = {
    val key = tpe.dealias // Use dealiased type as cache key

    if (IzumiReflectConfig.enableSerializedLttCache) {
      serializedLttCache.get(key).flatMap(ref => Option(ref.get())) match {
        case Some(cachedSerialized) =>
          logger.debug(s"LightTypeTagFactory: Serialized LTT Cache HIT for type $tpe (key: $key)")
          q"_root_.izumi.reflect.macr.LightTypeTag.parse(${Literal(Constant(cachedSerialized))})"
        case None =>
          logger.debug(s"LightTypeTagFactory: Serialized LTT Cache MISS for type $tpe (key: $key)")
          val ltt = buildLightTypeTag(c)(tpe)
          val serialized = LightTypeTag.serialize(ltt)
          serializedLttCache.put(key, new SoftReference(serialized))
          q"_root_.izumi.reflect.macr.LightTypeTag.parse(${Literal(Constant(serialized))})"
      }
    } else if (IzumiReflectConfig.enableLightTypeTagCache) {
      lttCache.get(key).flatMap(ref => Option(ref.get())) match {
        case Some(cachedLtt) =>
          logger.debug(s"LightTypeTagFactory: LTT Cache HIT for type $tpe (key: $key)")
          q"${Literal(Constant(cachedLtt))}"
        case None =>
          logger.debug(s"LightTypeTagFactory: LTT Cache MISS for type $tpe (key: $key)")
          val ltt = buildLightTypeTag(c)(tpe)
          lttCache.put(key, new SoftReference(ltt))
          q"${Literal(Constant(ltt))}"
      }
    } else {
      logger.debug(s"LightTypeTagFactory: LTT-level caches disabled for type $tpe")
      buildLightTypeTag(c)(tpe)
    }
  }

  private def buildLightTypeTag(c: IzumiReflectMacroContext)(tpe: Type): Tree = {
    val typeRef = new LightTypeTagRef(c)(tpe)

    val basesDb = if (IzumiReflectConfig.enableBasesDbCache) {
      basesDbCache.get(tpe).flatMap(ref => Option(ref.get())) match {
        case Some(cachedBasesDb) =>
          logger.debug(s"LightTypeTagFactory: BasesDb Cache HIT for type $tpe")
          cachedBasesDb
        case None =>
          logger.debug(s"LightTypeTagFactory: BasesDb Cache MISS for type $tpe")
          val db = typeRef.basesDb
          basesDbCache.put(tpe, new SoftReference(db))
          db
      }
    } else {
      logger.debug(s"LightTypeTagFactory: BasesDb cache disabled for type $tpe")
      typeRef.basesDb
    }

    val inheritanceDb = if (IzumiReflectConfig.enableInheritanceDbCache) {
      inheritanceDbCache.get(tpe).flatMap(ref => Option(ref.get())) match {
        case Some(cachedInheritanceDb) =>
          logger.debug(s"LightTypeTagFactory: InheritanceDb Cache HIT for type $tpe")
          cachedInheritanceDb
        case None =>
          logger.debug(s"LightTypeTagFactory: InheritanceDb Cache MISS for type $tpe")
          val db = typeRef.inheritanceDb
          inheritanceDbCache.put(tpe, new SoftReference(db))
          db
      }
    } else {
      logger.debug(s"LightTypeTagFactory: InheritanceDb cache disabled for type $tpe")
      typeRef.inheritanceDb
    }

    val (ref, parameters, outer, renames, methods, defs, ctors) = typeRef.extractTag(tpe)

    q"""
        new _root_.izumi.reflect.macr.LightTypeTag.Parsed(
          ${ref},
          _root_.scala.collection.immutable.List(..$parameters),
          ${outer},
          _root_.scala.collection.immutable.Map(..$renames),
          _root_.scala.collection.immutable.Set(..$methods),
          _root_.scala.collection.immutable.Set(..$defs),
          _root_.scala.collection.immutable.Set(..$ctors),
          _root_.scala.collection.immutable.Map(..${basesDb.map { case (k, v) => q"(${k}, _root_.scala.collection.immutable.Set(..$v))" }}),
          _root_.scala.collection.immutable.Map(..${inheritanceDb.map { case (k, v) => q"(${k}, _root_.scala.collection.immutable.Set(..$v))" }})
        )
     """
  }
}

object LightTypeTagFactory {
  private[reflect] final val logger = new TrivialLogger(
    "izumi.reflect.macr.tags.LightTypeTagFactory",
    // For a normal user, this should be disabled.
    // For a developer working on izumi-reflect, this should be enabled.
    // The value is hardcoded because it's only a debug setting.
    debug = false,
  )
}
