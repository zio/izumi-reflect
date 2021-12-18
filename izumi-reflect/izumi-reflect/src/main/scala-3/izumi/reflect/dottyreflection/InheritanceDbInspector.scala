package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef._
import scala.collection.mutable

import scala.quoted._

abstract class InheritanceDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector = new Inspector(0) { val qctx: InheritanceDbInspector.this.qctx.type = InheritanceDbInspector.this.qctx }

  def makeUnappliedInheritanceDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpe = TypeRepr.of[T].dealias

    val allReferenceComponents = allTypeReferences(tpe)

    val baseclassReferences = allReferenceComponents.flatMap {
      i =>
        val allbases = tpeBases(i).filter(!_._takesTypeArgs)
        val targetRef = {
          val tpef = i._fullNormDealiasSimplified._resultType
          inspector.makeNameReferenceFromType(tpef)
        }
        allbases.map(b => (targetRef, inspector.makeNameReferenceFromType(b._fullNormDealiasSimplified)))
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

    def extractComponents(tpeRaw0: TypeRepr): Unit = {
      val intersectionUnionMembers = breakRefinement(tpeRaw0._fullNormDealiasSimplified._resultType)

      if (intersectionUnionMembers.sizeIs == 1) {
        inh += intersectionUnionMembers.head
      }

      (
//        tpeRaw0._typeArgs.iterator ++
        intersectionUnionMembers.iterator.flatMap(_._typeArgs) ++
        intersectionUnionMembers
      ).foreach(t => if (!inh(t) && !ignored(t)) extractComponents(t))
    }

    extractComponents(tpe0)
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

  private def tpeBases(tpe0: TypeRepr): List[TypeRepr] = {
    val tpef = tpe0._fullNormDealiasSimplified._resultType
    val higherBases = tpef.baseClasses
    val onlyParameterizedBases = {
      higherBases
        .filterNot {
          s =>
            !s.isType || (s.tree match {
              case tt: TypeTree => tt.tpe =:= tpef
              case _ => false
            })
        }
        .map(s => tpef.baseType(s))
    }

    val allbases = onlyParameterizedBases.filterNot(_ =:= tpef)
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
