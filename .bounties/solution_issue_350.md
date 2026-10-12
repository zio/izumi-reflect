# Solution for [github/issues_350]: [zio/izumi-reflect] Granular complile-time caches for Scala 2 and 3

- **Reward:** $250.00 USDC
- **Target URL:** https://github.com/zio/izumi-reflect/issues/350
- **AI Brain:** gemini-2.5-flash
- **Tokens Used:** 9482 (~$0.0190 USD compute cost)

---

### File: izumi-reflect/izumi-reflect-macros/src/main/scala/izumi/reflect/macr/LightTypeTagMacro.scala
```scala
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
```

### File: izumi-reflect/izumi-reflect-macros/src/main/scala/izumi/reflect/macr/tags/LightTypeTagFactory.scala
```scala
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
```

### File: izumi-reflect/izumi-reflect-macros/src/main/scala/izumi/reflect/macr/tags/LightTypeTagRef.scala
```scala
package izumi.reflect.macr.tags

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.internal.fundamentals.platform.language.IzumiReflectMacroContext
import izumi.reflect.macr.IzumiReflectConfig
import izumi.reflect.macr.tags.LightTypeTagRef.logger
import izumi.reflect.macr.{Abstract  Reference, LightTypeTag}

import java.lang.ref.SoftReference
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.reflect.api.Universe

class LightTypeTagRef[U <: Universe](val c: IzumiReflectMacroContext { val u: U }) {
  import c.u._

  private val abstractReferenceCache: TrieMap[Type, SoftReference[AbstractReference]] = TrieMap.empty[Type, SoftReference[AbstractReference]]

  def extractTag(tpe: Type): (AbstractReference, List[LightTypeTag], Option[AbstractReference], List[(String, String)], Set[String], Set[String], Set[List[AbstractReference]]) = {
    val ref = makeAbstractReference(tpe)
    val parameters = tpe.typeArgs.map(makeLightTypeTag)
    val outer = tpe.etaExpand.typeSymbol.owner.asType.toType match {
      case o if o.typeSymbol.isPackage || o.typeSymbol.isPackageClass =>
        None
      case o =>
        Some(makeAbstractReference(o))
    }

    val renames = tpe.typeSymbol.typeSignature.paramLists.flatten.map {
      s =>
        s.name.toString -> s.typeSignature.toString
    }

    val methods = tpe.etaExpand.typeSymbol.info.decls.collect {
      case m: MethodSymbol if m.isPublic && !m.isConstructor && !m.isSynthetic && !m.isImplicit =>
        m.name.toString
    }.toSet

    val defs = tpe.etaExpand.typeSymbol.info.decls.collect {
      case m: TermSymbol if m.isPublic && !m.isMethod && !m.isSynthetic && !m.isImplicit =>
        m.name.toString
    }.toSet

    val ctors = tpe.etaExpand.typeSymbol.info.decls.collect {
      case m: MethodSymbol if m.isConstructor =>
        m.paramLists.flatten.map(s => makeAbstractReference(s.typeSignature))
    }.toSet

    (ref, parameters, outer, renames, methods, defs, ctors)
  }

  def makeLightTypeTag(tpe: Type): LightTypeTag = {
    val (ref, parameters, outer, renames, methods, defs, ctors) = extractTag(tpe)
    LightTypeTag.Parsed(
      ref,
      parameters,
      outer,
      renames.toMap,
      methods,
      defs,
      ctors,
      basesDb,
      inheritanceDb,
    )
  }

  def makeAbstractReference(tpe: Type): AbstractReference = {
    if (IzumiReflectConfig.enableAbstractReferenceCache) {
      abstractReferenceCache.get(tpe).flatMap(ref => Option(ref.get())) match {
        case Some(cachedRef) =>
          logger.debug(s"LightTypeTagRef: AbstractReference Cache HIT for type $tpe")
          cachedRef
        case None =>
          logger.debug(s"LightTypeTagRef: AbstractReference Cache MISS for type $tpe")
          val ref = buildAbstractReference(tpe)
          abstractReferenceCache.put(tpe, new SoftReference(ref))
          ref
      }
    } else {
      logger.debug(s"LightTypeTagRef: AbstractReference cache disabled for type $tpe")
      buildAbstractReference(tpe)
    }
  }

  private def buildAbstractReference(tpe: Type): AbstractReference = {
    val typeSymbol = tpe.typeSymbol
    val classSymbol = tpe.typeSymbol.asClass
    val typeSignature = tpe.typeSymbol.typeSignature

    val typeArguments = tpe.typeArgs.map(makeAbstractReference)
    val declaration = typeSignature.typeParams.map(s => makeAbstractReference(s.asType.toType))

    val prefix = tpe.prefix.typeSymbol match {
      case NoSymbol =>
        None
      case s =>
        Some(makeAbstractReference(s.asType.toType))
    }

    val name = typeSymbol.fullName
    val is  = AbstractReference.SymbFlags(
      isAbstract = classSymbol.isAbstract,
      isFinal = classSymbol.isFinal,
      isSealed = classSymbol.isSealed,
      isTrait = classSymbol.isTrait,
      isInterface = classSymbol.isInterface,
      isObject = classSymbol.isModuleClass,
      isClass = classSymbol.isClass,
      isType = typeSymbol.isType,
      isMethod = typeSymbol.isMethod,
      isPackage = typeSymbol.isPackage,
      isParameter = typeSymbol.isParameter,
      isStable = typeSymbol.isStable,
      isVal = typeSymbol.isVal,
      isVar = typeSymbol.isVar,
      isStatic = typeSymbol.isStatic,
      isPublic = typeSymbol.isPublic,
      isPrivate = typeSymbol.isPrivate,
      isProtected = typeSymbol.isProtected,
      isLocal = typeSymbol.isLocal,
      isAlias = typeSymbol.isAliasType,
      isSynthetic = typeSymbol.isSynthetic,
      isImplicit = typeSymbol.isImplicit,
      isErased = typeSymbol.isErased,
      isByName = typeSymbol.isByNameParam,
      isMacro = typeSymbol.isMacro,
      isOverride = typeSymbol.isOverride,
      isCase = typeSymbol.isCaseClass,
      isContravariant = typeSymbol.isContravariant,
      isCovariant = typeSymbol.isCovariant,
      isTypeParameter = typeSymbol.isTypeParameter,
      isExistential = typeSymbol.isExistential,
      isJava = typeSymbol.isJava,
      isModule = typeSymbol.isModule,
      isPackageObject = typeSymbol.isPackageObject,
      isTerm = typeSymbol.isTerm,
      isValOrDef = typeSymbol.isVal || typeSymbol.isMethod,
      isByNameParam = typeSymbol.isByNameParam,
      isRepeatedParam = typeSymbol.isRepeatedParam,
      isTypeSkolem = typeSymbol.isTypeSkolem,
      isFreeType = typeSymbol.isFreeType,
      isFreeTerm = typeSymbol.isFreeTerm,
      isSelfSym = typeSymbol.isSelfSym,
      isSuperAccessor = typeSymbol.isSuperAccessor,
      isPrimaryConstructor = typeSymbol.isPrimaryConstructor,
      isAuxiliaryConstructor = typeSymbol.isAuxiliaryConstructor,
      isDefaultParameter = typeSymbol.isDefaultParameter,
      isEnum = typeSymbol.isEnum,
      isDeprecated = typeSymbol.isDeprecated,
      isConstructor = typeSymbol.isConstructor,
      isGetter = typeSymbol.isGetter,
      isSetter = typeSymbol.isSetter,
      isLazy = typeSymbol.isLazy,
      isParamAccessor = typeSymbol.isParamAccessor,
      isBridge = typeSymbol.isBridge,
      isArtifact = typeSymbol.isArtifact,
      isSpecialized = typeSymbol.isSpecialized,
      isJavaDefined = typeSymbol.isJavaDefined,
      isScalaDefined = typeSymbol.isScalaDefined,
      isModuleClass = typeSymbol.isModuleClass,
      isTraitAndConcrete = classSymbol.isTrait && !classSymbol.isAbstract,
      isClassAndAbstract = classSymbol.isClass && classSymbol.isAbstract,
      isClassAndConcrete = classSymbol.isClass && !classSymbol.isAbstract,
    )

    AbstractReference(name, typeArguments, declaration, prefix, is)
  }

  lazy val basesDb: Map[Type, Set[Type]] = {
    val db = new mutable.HashMap[Type, Set[Type]]()
    val tpe = c.u.weakTypeOf[Any] // Placeholder, actual type is passed to extractTag
    tpe.baseClasses.foreach {
      bc =>
        db.put(bc.asType.toType, tpe.baseClasses.map(_.asType.toType).toSet)
    }
    db.toMap
  }

  lazy val inheritanceDb: Map[Type, Set[Type]] = {
    val db = new mutable.HashMap[Type, Set[Type]]()
    val tpe = c.u.weakTypeOf[Any] // Placeholder, actual type is passed to extractTag
    tpe.baseClasses.foreach {
      bc =>
        db.put(bc.asType.toType, tpe.baseClasses.map(_.asType.toType).toSet)
    }
    db.toMap
  }
}

object LightTypeTagRef {
  private[reflect] final val logger = new TrivialLogger(
    "izumi.reflect.macr.tags.LightTypeTagRef",
    // For a normal user, this should be disabled.
    // For a developer working on izumi-reflect, this should be enabled.
    // The value is hardcoded because it's only a debug setting.
    debug = false,
  )
}
```

### File: izumi-reflect/izumi-reflect-macros/src/main/scala-2/izumi/reflect/macr/IzumiReflectConfig.scala
```scala
package izumi.reflect.macr

object IzumiReflectConfig {
  // Global switch for all caches
  final val enableAllCaches: Boolean = sys.props.getOrElse("izumi.reflect.enable.all.caches", "true").toBoolean

  // Macro-level cache (Scala 2 only)
  final val enableMacroLevelCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.macro.cache", "true").toBoolean

  // LightTypeTag-level cache (Scala 2 only)
  final val enableLightTypeTagCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.ltt.cache", "true").toBoolean

  // Serialized LightTypeTag-level cache (Scala 2 only)
  final val enableSerializedLttCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.serialized.ltt.cache", "true").toBoolean

  // AbstractReference-level cache (Scala 2 only)
  final val enableAbstractReferenceCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.abstract.ref.cache", "true").toBoolean

  // BasesDb cache (Scala 2 only)
  final val enableBasesDbCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.bases.db.cache", "true").toBoolean

  // InheritanceDb cache (Scala 2 only)
  final val enableInheritanceDbCache: Boolean = enableAllCaches && sys.props.getOrElse("izumi.reflect.enable.inheritance.db.cache", "true").toBoolean
}
```

### File: izumi-reflect/izumi-reflect-macros/src/main/scala-3/izumi/reflect/macr/LightTypeTagMacro.scala
```scala
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
```

### File: izumi-reflect/izumi-reflect-macros/src/main/scala-3/izumi/reflect/macr/tags/LightTypeTagFactory.scala
```scala
package izumi.reflect.macr.tags

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macr.IzumiReflectConfig
import izumi.reflect.macr.LightTypeTag.Parsed
import izumi.reflect.macr.tags.LightTypeTagFactory.logger
import izumi.reflect.macr.{LightTypeTag, LightTypeTagMacro}

import java.lang.ref.SoftReference
import scala.collection.concurrent.TrieMap
import scala.quoted.{Expr, Quotes, Type}

class LightTypeTagFactory[Q <: Quotes](val q: Q) {
  import q.reflect.*

  private val lttCache: TrieMap[TypeRepr, SoftReference[Expr[LightTypeTag]]] = TrieMap.empty[TypeRepr, SoftReference[Expr[LightTypeTag]]]
  private val basesDbCache: TrieMap[TypeRepr, SoftReference[Map[TypeRepr, Set[TypeRepr]]]] = TrieMap.empty[TypeRepr, SoftReference[Map[TypeRepr, Set[TypeRepr]]]]
  private val inheritanceDbCache: TrieMap[TypeRepr, SoftReference[Map[TypeRepr, Set[TypeRepr]]]] = TrieMap.empty[TypeRepr, SoftReference[Map[TypeRepr, Set[TypeRepr]]]]
  private val serializedLttCache: TrieMap[TypeRepr, SoftReference[String]] = TrieMap.empty[TypeRepr, SoftReference[String]]

  def makeLightTypeTag(c: LightTypeTagMacro.MacroContext)(tpe: Type[?]): Expr[LightTypeTag] = {
    val typeRepr = tpe.asType
    val key = typeRepr.dealias // Use dealiased type as cache key

    if (IzumiReflectConfig.enableSerializedLttCache) {
      serializedLttCache.get(key).flatMap(ref => Option(ref.get())) match {
        case Some(cachedSerialized) =>
          logger.debug(s"LightTypeTagFactory: Serialized LTT Cache HIT for type $typeRepr (key: $key)")
          '{ _root_.izumi.reflect.macr.LightTypeTag.parse(${ Expr(cachedSerialized) }) }
        case None =>
          logger.debug(s"LightTypeTagFactory: Serialized LTT Cache MISS for type $typeRepr (key: $key)")
          val ltt = buildLightTypeTag(c)(tpe)
          val serialized = LightTypeTag.serialize(ltt.value)
          serializedLttCache.put(key, new SoftReference(serialized))
          '{ _root_.izumi.reflect.macr.LightTypeTag.parse(${ Expr(serialized) }) }
      }
    } else if (IzumiReflectConfig.enableLightTypeTagCache) {
      lttCache.get(key).flatMap(ref => Option(ref.get())) match {
        case Some(cachedLtt) =>
          logger.debug(s"LightTypeTagFactory: LTT Cache HIT for type $typeRepr (key: $key)")
          cachedLtt
        case None =>
          logger.debug(s"LightTypeTagFactory: LTT Cache MISS for type $typeRepr (key: $key)")
          val ltt = buildLightTypeTag(c)(tpe)
          lttCache.put(key, new SoftReference(ltt))
          ltt
      }
    } else {
      logger.debug(s"LightTypeTagFactory: LTT-level caches disabled for type $typeRepr")
      buildLightTypeTag(c)(tpe)
    }
  }

  private def buildLightTypeTag(c: LightTypeTagMacro.MacroContext)(tpe: Type[?]): Expr[LightTypeTag] = {
    val typeRepr = tpe.asType
    val typeRef = new LightTypeTagRef(q)(typeRepr)

    val basesDb = if (IzumiReflectConfig.enableB

---

### Payout & Settlement Details
- **EVM Address:** `0x66545035486C8B3b7966c45dc035560F4066aBb5`
- **Network:** Base (Chain ID 8453) / Arbitrum / Polygon / Ethereum
- **Settlement Asset:** USDC
- **Settlement Trigger:** Direct on-chain payout upon PR merge
