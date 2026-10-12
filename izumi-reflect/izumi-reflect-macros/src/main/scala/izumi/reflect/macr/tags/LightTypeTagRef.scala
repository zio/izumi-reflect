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
