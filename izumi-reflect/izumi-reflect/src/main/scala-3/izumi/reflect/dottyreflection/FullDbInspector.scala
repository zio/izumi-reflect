package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable
import scala.quoted._

abstract class FullDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector = new Inspector(0, List.empty) { val qctx: FullDbInspector.this.qctx.type = FullDbInspector.this.qctx }

  def buildFullDb[T <: AnyKind: Type]: Map[AbstractReference, Set[AbstractReference]] = {
    new Run()
      .inspectTypeReprToFullBases(TypeRepr.of[T])
      .distinct
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }

  class Run() {
    private val termination = mutable.HashSet.empty[TypeRepr]

    def inspectTypeReprToFullBases(tpe0: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      val tpe = tpe0.dealias.simplified
      lazy val selfRef = inspector.inspectTypeRepr(tpe) // FIXME duplicate work for top-level type

      tpe match {
        case t if ignored(t) =>
          Nil

        case a: AppliedType =>
          extractBase(a, selfRef, recurseIntoBases = false)

        case l: TypeLambda =>
          val parents = inspectTypeBoundsToFull(l.resType)
          val selfL = selfRef.asInstanceOf[LightTypeTagRef.Lambda]
          val out = parents.map {
            case (c, p) =>
              if (c == selfL.output) {
                (selfL, p)
              } else {
                (c, p)
              }
          }
          out.distinct

        case a: AndType =>
          inspectTypeReprToFullBases(a.left) ++ inspectTypeReprToFullBases(a.right)

        case o: OrType =>
          inspectTypeReprToFullBases(o.left) ++ inspectTypeReprToFullBases(o.right)

        case typeRef: TypeRef =>
          processSymbol(typeRef, selfRef)

        case paramRef: ParamRef =>
          processSymbol(paramRef, selfRef)

        case termRef: TermRef =>
          extractBase(termRef, selfRef, false)

        case b: TypeBounds =>
          inspectTypeReprToFullBases(b.hi) ++ inspectTypeReprToFullBases(b.low)

        case c: ConstantType =>
          extractBase(c, selfRef, false)

        case t: ThisType =>
          inspectTypeReprToFullBases(t.tref)

        case other =>
          log(s"FullDbInspector: UNSUPPORTED: $other")
          extractBase(other, selfRef, false)
      }
    }

    private def processSymbol(r: TypeRef | ParamRef, selfRef: AbstractReference): List[(AbstractReference, AbstractReference)] = {
      r.typeSymbol match {
        case s if s.isClassDef =>
          extractBase(r, selfRef, recurseIntoBases = true)

        case s if s.isTypeDef =>
          inspectTypeReprToFullBases(r._underlying)

        case o =>
          throw new RuntimeException(s"Shit tree: ${o.getClass} $o $r ${o.tree}")
      }
    }

    private def extractBase(tpe: TypeRepr, selfRef: AbstractReference, recurseIntoBases: Boolean): List[(AbstractReference, AbstractReference)] = {
      val baseTypes = tpe.baseClasses.iterator.map(tpe.baseType).filterNot(ignored).filterNot(termination).toList
      log(s"For `$tpe` found base types $baseTypes")

      val recursiveParentBases = if (recurseIntoBases) {
        baseTypes.filterNot(_ == tpe).flatMap {
          t =>
            inspectTypeReprToFullBases(t)
        }
      } else {
        List.empty
      }
      val main = recursiveParentBases ++ baseTypes.map {
        bt =>
          val parentRef = inspector.inspectTypeRepr(bt)
          (selfRef, parentRef)
      }

      val typeArgs: List[TypeRepr] = tpe match {
        case a: AppliedType =>
          a.args
        case _ =>
          val m = qctx.reflect.TypeReprMethods
          val mm = m.getClass.getMethods.collect { case m if m.getName == "typeArgs" => m }.head
          val out = mm.invoke(m, tpe).asInstanceOf[List[TypeRepr]]
          out
      }

      val argInheritance = typeArgs.filterNot(termination.contains).flatMap {
        x =>
          termination.add(x)
          inspectTypeBoundsToFull(x)
      }

      (main ++ argInheritance).distinct
    }

    private def inspectTypeBoundsToFull(tpe: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      tpe.dealias match {
        case t: TypeBounds =>
          inspectTypeReprToFullBases(t.hi) ++ inspectTypeReprToFullBases(t.low)
        case t: TypeRepr =>
          inspectTypeReprToFullBases(t)
      }
    }
  }

}
