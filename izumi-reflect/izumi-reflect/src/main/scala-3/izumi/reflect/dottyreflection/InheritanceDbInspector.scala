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

  private lazy val inspector = Inspector.make(qctx)

  def makeUnappliedInheritanceDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpe0 = TypeRepr.of[T]._dealiasSimplifiedFull

    new Run()
      .makeUnappliedInheritanceDb(tpe0)
  }

  class Run() {
    private val termination = mutable.HashSet.empty[TypeRepr]

    def makeUnappliedInheritanceDb(tpe0: TypeRepr): Map[NameReference, Set[NameReference]] = {
      val allReferenceComponents = allTypeReferences(tpe0).filter {
        case _: ParamRef => false // do not process type parameters for inheritance db
        case _ => true
      }

      val baseclassReferences = allReferenceComponents.flatMap(inspectTypeReprToUnappliedBases)

      baseclassReferences
        .toMultimap
        .map {
          case (t, parents) =>
            t -> parents.filterNot(_ == t)
        }
        .filterNot(_._2.isEmpty)
    }

    private def inspectTypeReprToUnappliedBases(i: TypeRepr): List[(NameReference, NameReference)] = {
      val tpe = i._dealiasSimplifiedFull._resultType
      val tpeRef = inspector.makeNameReferenceFromType(tpe)

      tpeBases(tpeRef, tpe)
    }

    private def allTypeReferences(tpe0: TypeRepr): collection.Set[TypeRepr] = {
      val inh = mutable.HashSet.empty[TypeRepr]

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
//    println(inh)

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

    private def tpeBases(tpeRef: NameReference, typeRepr: TypeRepr): List[(NameReference, NameReference)] = {
      termination.add(typeRepr)

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

      val baseTypeBases = (upperBoundBases ++ typeReprBases).filterNot(_ =:= typeRepr)

      val recursiveParentBases = baseTypeBases.flatMap {
        case t if termination.contains(t) => Nil
        case t => inspectTypeReprToUnappliedBases(t)
      }

      val mainBases = baseTypeBases.filter(!_._takesTypeArgs).map(base => (tpeRef, inspector.makeNameReferenceFromType(base)))

      recursiveParentBases ++ mainBases
    }

  }

}
