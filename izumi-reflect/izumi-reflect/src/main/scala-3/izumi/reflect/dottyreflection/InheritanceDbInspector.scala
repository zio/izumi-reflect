package izumi.reflect.dottyreflection

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.*

import java.lang.ref.SoftReference
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.quoted.*

object InheritanceDbInspector {
  private val dbCache = new ConcurrentHashMap[String, SoftReference[Map[NameReference, Set[NameReference]]]]()
  private val cacheHits = new java.util.concurrent.atomic.AtomicLong(0)
  private val cacheMisses = new java.util.concurrent.atomic.AtomicLong(0)

  def getCacheStats: (Long, Long, Int) = (cacheHits.get(), cacheMisses.get(), dbCache.size())
  def resetCacheStats(): Unit = { cacheHits.set(0); cacheMisses.set(0) }

  private def dbCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  def make(q: Quotes): InheritanceDbInspector { val qctx: q.type } = new InheritanceDbInspector(0) {
    override val qctx: q.type = q
  }
}

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect.*

  def makeUnappliedInheritanceDb(typeRepr: TypeRepr): Map[NameReference, Set[NameReference]] = {
    val key = makeStructuralKey(typeRepr)

    val cachedResult: Option[Map[NameReference, Set[NameReference]]] =
      if (InheritanceDbInspector.dbCacheEnabled) {
        Option(InheritanceDbInspector.dbCache.get(key)).flatMap(sr => Option(sr.get()))
      } else {
        None
      }

    cachedResult match {
      case Some(cached) =>
        InheritanceDbInspector.cacheHits.incrementAndGet()
        cached
      case None =>
        InheritanceDbInspector.cacheMisses.incrementAndGet()
        val tpe0 = typeRepr._dealiasSimplifiedFull

        val result = new Run(Inspector.make(qctx), mutable.HashSet.empty)
          .makeUnappliedInheritanceDb(tpe0)

        if (InheritanceDbInspector.dbCacheEnabled) {
          InheritanceDbInspector.dbCache.put(key, new SoftReference(result))
        }

        result
    }
  }

  private def makeStructuralKey(typeRepr: TypeRepr): String = {
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

  class Run(
    inspector: Inspector { val qctx: InheritanceDbInspector.this.qctx.type },
    termination: mutable.HashSet[Symbol]
  ) {

    def makeUnappliedInheritanceDb(tpe0: TypeRepr): Map[NameReference, Set[NameReference]] = {
      inspectTypeReprToUnappliedBases(tpe0, onlyIndirect = false)
        .iterator
        .filterNot {
          case (parent, t) =>
            parent == t
        }
        .toMultimap
    }

    private def inspectTypeReprToUnappliedBases(tpe0: TypeRepr, onlyIndirect: Boolean): List[(NameReference, NameReference)] = {
      val allReferenceComponents = allTypeReferences(tpe0, onlyIndirect)
      allReferenceComponents.iterator.flatMap(inspectTypeReprToUnappliedIndirectBases).toList
    }

    private def inspectTypeReprToUnappliedIndirectBases(i: TypeRepr): List[(NameReference, NameReference)] = {
      val tpe = i._dealiasSimplifiedFull._resultType
      val tpeRef = inspector.makeNameReferenceFromType(tpe)

      tpeBases(tpeRef, tpe, onlyIndirect = false)
    }

    private def allTypeReferences(tpe0: TypeRepr, onlyIndirect: Boolean): mutable.Set[TypeRepr] = {
      extension (t: TypeRepr) {
        inline def dealiasPrepare: TypeRepr = {
          t._dealiasSimplifiedFull._resultType
        }
      }

      val inh = mutable.LinkedHashSet.empty[TypeRepr]

      val tpeDealiased = tpe0.dealiasPrepare

      def goExtractComponents(tpeRaw0: TypeRepr): Unit = {
        val tpeRes = tpeRaw0.dealiasPrepare
        val intersectionUnionMembers = breakRefinement(tpeRes)

        if (intersectionUnionMembers.sizeIs == 1) {
          inh += intersectionUnionMembers.head
        }

        (
          tpeRes.typeArgs.iterator ++
          intersectionUnionMembers.iterator.flatMap(_.typeArgs) ++
          intersectionUnionMembers
        ).foreach(t => if (!inh.contains(t)) goExtractComponents(t))
      }

      goExtractComponents(tpe0)

      inh.filterInPlace {
        case _: ParamRef => false // do not process type parameters for inheritance db
        case t if onlyIndirect => t != tpe0 && t != tpeDealiased && !isTerminatingClsSym(t)
        case _ => true
      }

      inh
    }

    private def breakRefinement(tpe0: TypeRepr): collection.Set[TypeRepr] = {
      val tpes = mutable.LinkedHashSet.empty[TypeRepr]

      def go(t0: TypeRepr): Unit = t0._dealiasSimplifiedFull match {
        case tpe: AndOrType =>
          go(tpe.left)
          go(tpe.right)
        case r: Refinement =>
          refinementInfoToParts(r.info).foreach(go)
          go(r.parent)
        case t =>
          tpes += t
      }

      go(tpe0)
      tpes
    }

    private def tpeBases(tpeRef: NameReference, typeRepr: TypeRepr, onlyIndirect: Boolean): List[(NameReference, NameReference)] = {
      addTerminatingClsSym(typeRepr)

      val typeReprBases = typeRepr
        .baseClasses
        .map(typeRepr.baseType)

      val upperBoundBases = typeRepr match {
        case t: TypeRef =>
          t._underlying match {
            // handle abstract higher-kinded type members specially,
            // move their upper bound into inheritance db, because they
            // will lose it after application. (Unlike proper type members)
            case TypeBounds(_, tl: TypeLambda) =>
              List(tl.resType._dealiasSimplifiedFull)
            case _ =>
              Nil
          }
        case _ =>
          Nil
      }

      val allTypeReprBases = (upperBoundBases ::: typeReprBases)
        .filterNot(_ =:= typeRepr)

      val recursiveParentRefs = allTypeReprBases.flatMap {
        case t if isTerminatingClsSym(t) => Nil
        case t => inspectTypeReprToUnappliedBases(t, onlyIndirect = true)
      }

      val directBaseRefs = if (onlyIndirect) {
        Nil
      } else {
        allTypeReprBases.filter(!_._takesTypeArgs).map(base => (tpeRef, inspector.makeNameReferenceFromType(base)))
      }

      recursiveParentRefs ::: directBaseRefs
    }

    private def addTerminatingClsSym(typeRepr: TypeRepr): Unit = {
      typeRepr.classSymbol match {
        case Some(clsSym) => termination.add(clsSym)
        case _ =>
      }
    }

    private def isTerminatingClsSym(t: TypeRepr): Boolean = {
      t.classSymbol match {
        case Some(clsSym) => termination.contains(clsSym)
        case None => false
      }
    }

  }

}
