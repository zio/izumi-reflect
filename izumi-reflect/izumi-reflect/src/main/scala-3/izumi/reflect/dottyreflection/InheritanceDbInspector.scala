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
  private type TypeReprKey = Quotes#reflectModule#TypeRepr
  private val dbCache = new ConcurrentHashMap[TypeReprKey, SoftReference[Map[NameReference, Set[NameReference]]]]()

  private def compileCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`))
      .flatMap(_.asBoolean())
      .getOrElse(true)
  }

  private def inheritanceDbCacheEnabled: Boolean = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
    Option(System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile.db.inheritance`))
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
    val cacheEnabled = InheritanceDbInspector.compileCacheEnabled && InheritanceDbInspector.inheritanceDbCacheEnabled
    val key: InheritanceDbInspector.TypeReprKey = typeRepr.dealias.simplified

    val cachedResult: Option[Map[NameReference, Set[NameReference]]] =
      if (cacheEnabled) {
        Option(InheritanceDbInspector.dbCache.get(key)).flatMap(sr => Option(sr.get()))
      } else {
        None
      }

    cachedResult match {
      case Some(cached) =>
        cached
      case None =>
        val tpe0 = typeRepr._dealiasSimplifiedFull

        val result = new Run(Inspector.make(qctx), mutable.HashSet.empty)
          .makeUnappliedInheritanceDb(tpe0)

        if (cacheEnabled) {
          InheritanceDbInspector.dbCache.put(key, new SoftReference(result))
        }

        result
    }
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
