package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef._
import scala.collection.mutable

import scala.quoted._

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector = new Inspector(0, List.empty) { val qctx: InheritanceDbInspector.this.qctx.type = InheritanceDbInspector.this.qctx }

  def makeUnappliedInheritanceDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpe0 = TypeRepr.of[T].dealias

    val allReferenceComponents = allTypeReferences(tpe0)

    val baseclassReferences = allReferenceComponents.flatMap {
      i =>
        val tpef = i.dealias.simplified._resultType
        val allbases = tpeBases(tpef).filter(!_._takesTypeArgs)
        val targetRef = inspector.makeNameReferenceFromType(tpef, assertContextCorrectness = false)
        allbases.map(b => (targetRef, inspector.makeNameReferenceFromType(b, assertContextCorrectness = false)))
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
      ).foreach(t => if (!inh(t) && !ignored(t)) goExtractComponents(t))
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
        .filter(s => s.isType)
        .map(s => typeRepr.baseType(s))

    val allbases = onlyParameterizedBases.filterNot(_ =:= typeRepr)

    allbases
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
