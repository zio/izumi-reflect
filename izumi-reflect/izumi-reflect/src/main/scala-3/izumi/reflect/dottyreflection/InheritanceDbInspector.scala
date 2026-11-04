package izumi.reflect.dottyreflection

import izumi.reflect.dottyreflection.CacheStats.{SoftWeakCache, TypeReprKey}
import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.collection.mutable
import scala.quoted.*

object InheritanceDbInspector {
  private val dbCache = new SoftWeakCache[TypeReprKey, Map[NameReference, Set[NameReference]]]()
  private val componentCache = new SoftWeakCache[TypeReprKey, List[(NameReference, NameReference)]]()

  def make(q: Quotes): InheritanceDbInspector { val qctx: q.type } = new InheritanceDbInspector(0) {
    override val qctx: q.type = q
  }

  private[InheritanceDbInspector] def getCachedInheritanceDbForComponent(q: Quotes)(
    typeRepr: q.reflect.TypeRepr
  ): Option[List[(NameReference, NameReference)]] = {
    val cacheEnabled = CacheStats.compileCacheEnabled && CacheStats.inheritanceDbCacheEnabled
    if (cacheEnabled) {
      val key: TypeReprKey = typeRepr.dealias.simplified
      componentCache
        .get(key)
        .orElse {
          dbCache.get(key).map(_.iterator.flatMap { case (k, vs) => vs.map(v => (k, v)) }.toList)
        }
        .map {
          cached =>
            CacheStats.inheritanceDbHit()
            cached
        }
    } else {
      None
    }
  }

  private[InheritanceDbInspector] def putCachedComponent(q: Quotes)(
    typeRepr: q.reflect.TypeRepr,
    entries: List[(NameReference, NameReference)]
  ): Unit = {
    val cacheEnabled = CacheStats.compileCacheEnabled && CacheStats.inheritanceDbCacheEnabled
    if (cacheEnabled) {
      val key: TypeReprKey = typeRepr.dealias.simplified
      componentCache.put(key, entries)
    }
  }
}

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect.*

  def makeUnappliedInheritanceDb(typeRepr: TypeRepr): Map[NameReference, Set[NameReference]] = {
    val cacheEnabled = CacheStats.compileCacheEnabled && CacheStats.inheritanceDbCacheEnabled
    val key: TypeReprKey = typeRepr.dealias.simplified

    if (cacheEnabled) {
      InheritanceDbInspector.dbCache.get(key) match {
        case Some(cached) =>
          CacheStats.inheritanceDbHit()
          return cached
        case None =>
          CacheStats.inheritanceDbMiss()
      }
    }

    val tpe0 = typeRepr._dealiasSimplifiedFull

    val result = new Run(Inspector.make(qctx), mutable.HashSet.empty)
      .makeUnappliedInheritanceDb(tpe0)

    if (cacheEnabled) {
      InheritanceDbInspector.dbCache.put(key, result)
    }

    result
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

      if (inspector.context.isEmpty) {
        InheritanceDbInspector.getCachedInheritanceDbForComponent(qctx)(tpe) match {
          case Some(cachedEntries) =>
            addTerminatingClsSym(tpe)
            return cachedEntries
          case None =>
        }
      }

      val tpeRef = inspector.makeNameReferenceFromType(tpe)
      val entries = tpeBases(tpeRef, tpe, onlyIndirect = false)
      if (inspector.context.isEmpty) {
        InheritanceDbInspector.putCachedComponent(qctx)(tpe, entries)
      }
      entries
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
