package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.quoted.*

object InheritanceDbInspector {
  def make(q: Quotes): InheritanceDbInspector { val qctx: q.type } = new InheritanceDbInspector(0) {
    override val qctx: q.type = q
  }
}

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector = Inspector.make(qctx)

  def makeUnappliedInheritanceDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpe0 = TypeRepr.of[T].dealias

    val allReferenceComponents = allTypeReferences(tpe0).filter {
      case _: ParamRef => false // do not process type parameters for inheritance db
      case _ => true
    }

    val baseclassReferences = allReferenceComponents.flatMap {
      i =>
        val tpef = i.dealias.simplified._resultType
        val targetRef = inspector.makeNameReferenceFromType(tpef)

        val allbases = tpeBases(tpef).filter(!_._takesTypeArgs)
        allbases.map(b => (targetRef, inspector.makeNameReferenceFromType(b)))
    }

    baseclassReferences
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }

  private def allTypeReferences(tpe0: TypeRepr): collection.Set[TypeRepr] = {
    val inh = mutable.HashSet.empty[TypeRepr]

    def goExtractComponents(tpeRaw0: TypeRepr): Unit = {
      val tpeRes = tpeRaw0.dealias.simplified._resultType
      val intersectionUnionMembers = breakRefinement(tpeRes)

      if (intersectionUnionMembers.sizeIs == 1) {
        inh += intersectionUnionMembers.head
      }

      (
        tpeRes._typeArgs.iterator ++
        intersectionUnionMembers.iterator.flatMap(_._typeArgs) ++
        intersectionUnionMembers
      ).foreach(t => if (!inh.contains(t)) goExtractComponents(t))
    }

    goExtractComponents(tpe0)

    inh
  }

  private def breakRefinement(tpe0: TypeRepr): collection.Set[TypeRepr] = {
    val tpes = mutable.HashSet.empty[TypeRepr]

    def go(t0: TypeRepr): Unit = t0.dealias match {
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

  private def tpeBases(typeRepr: TypeRepr): List[TypeRepr] = {
    val onlyParameterizedBases =
      typeRepr
        .baseClasses
        .filter(_.isType)
        .map(typeRepr.baseType)

    val allbases = onlyParameterizedBases.filterNot(_ =:= typeRepr)

    val upperBoundBases = typeRepr match {
      case t: TypeRef =>
        t._underlying match {
          // handle abstract higher-kinded type members specially,
          // move their upper bound into inheritance db, because they
          // will lose it after application. (Unlike proper type members)
          case TypeBounds(_, tl: TypeLambda) =>
            List(tl.resType.dealias.simplified)
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }

    upperBoundBases ++ allbases
  }

  extension (t: TypeRepr) {
    private def _resultType: TypeRepr = {
      t match {
        case l: LambdaType => l.resType
        case _ => t
      }
    }

    private def _typeArgs: List[TypeRepr] = {
      t match {
        case a: AppliedType => a.args
        case _ => Nil
      }
    }

    private def _takesTypeArgs: Boolean = {
      t match {
        case l: LambdaType => true
        case _ => false
      }
    }
  }

}
