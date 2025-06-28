package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.quoted.*

object InheritanceDbInspector {
  def make(q: Quotes): InheritanceDbInspector { val qctx: q.type } = new InheritanceDbInspector(0) {
    override val qctx: q.type = q
  }
}

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect.*

  def makeUnappliedInheritanceDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpe0 = TypeRepr.of[T]._dealiasSimplifiedFull

    new Run(Inspector.make(qctx), mutable.HashSet.empty)
      .makeUnappliedInheritanceDb(tpe0)
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
      allReferenceComponents.iterator.flatMap(inspectTypeReprToUnappliedBases0(_, onlyIndirect = false)).toList
    }

    private def inspectTypeReprToUnappliedBases0(i: TypeRepr, onlyIndirect: Boolean): List[(NameReference, NameReference)] = {
      val tpe = i._dealiasSimplifiedFull._resultType
      val tpeRef = inspector.makeNameReferenceFromType(tpe)

      tpeBases(tpeRef, tpe, onlyIndirect = onlyIndirect)
    }

    private def allTypeReferences(tpe0: TypeRepr, onlyIndirect: Boolean): mutable.Set[TypeRepr] = {
      val inh = mutable.HashSet.empty[TypeRepr]

      val tpeDealiased = tpe0._dealiasSimplifiedFull._resultType

      def goExtractComponents(tpeRaw0: TypeRepr): Unit = {
        val tpeRes = tpeRaw0._dealiasSimplifiedFull._resultType
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
      val tpes = mutable.HashSet.empty[TypeRepr]

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
