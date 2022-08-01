package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable
import scala.quoted._

abstract class FullDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector = new Inspector(0) { val qctx: FullDbInspector.this.qctx.type = FullDbInspector.this.qctx }

  def buildFullDb[T <: AnyKind: Type]: Map[AbstractReference, Set[AbstractReference]] = {
    val uns = TypeTree.of[T]
    new Run()
      .inspectTreeToFull(uns)
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }

  class Run() {
    private val termination = mutable.HashSet.empty[TypeRepr]

    def inspectTreeToFull(tpeTree: TypeTree): List[(AbstractReference, AbstractReference)] = {
      inspectTypeReprToFullBases(tpeTree.tpe).distinct
    }

    private def inspectTypeReprToFullBases(tpe0: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      val tpe = tpe0.dealias
      lazy val selfRef = inspector.inspectTypeRepr(tpe)

      tpe match {
        case t if ignored(t) =>
          Nil

        case a: AppliedType =>
          extractBase(tpe, selfRef, recurseIntoBases = false)

        case l: TypeLambda =>
          val parents = inspectToBToFull(l.resType)
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

        case r: TypeRef =>
          processSymbol(r, selfRef)

        case r: ParamRef =>
          processSymbol(r, selfRef)

        case b: TypeBounds =>
          inspectToBToFull(b)

        case c: ConstantType =>
          inspectTypeReprToFullBases(c.widen)

        case o =>
          log(s"FullDbInspector: UNSUPPORTED: $o")
          Nil
      }
    }

    private def processSymbol(r: TypeRepr, selfRef: AbstractReference) = {
      r.typeSymbol match {
        case s if s.isClassDef =>
          extractBase(r, selfRef, recurseIntoBases = true)
        case _ =>
          // inspectTypeReprToFullBases(r.qualifier.memberType(r.typeSymbol))
          // FIXME below still required for non `subtype check fails when child type has absorbed a covariant type parameter of the supertype`
          inspectSymbolToFull(r.typeSymbol)
      }
    }

    private def extractBase(tpe: TypeRepr, selfRef: AbstractReference, recurseIntoBases: Boolean) = {
      val baseTypes = tpe.baseClasses.iterator.map(tpe.baseType).filterNot(ignored).filterNot(termination).toList
      log(s"For `$tpe` found base types $baseTypes")

      val rec = if (recurseIntoBases) {
        baseTypes.filterNot(_ == tpe).flatMap {
          t =>
            inspectTypeReprToFullBases(t)
        }
      } else {
        List.empty
      }
      val main = List((selfRef, selfRef)) ++ rec ++ baseTypes.map {
        bt =>
          val parentRef = inspector.inspectTypeRepr(bt)
          (selfRef, parentRef)
      }

      val args: List[TypeRepr] = tpe match {
        case a: AppliedType =>
          a.args
        case _ =>
          val m = qctx.reflect.TypeReprMethods
          val mm = m.getClass.getMethods.collect { case m if m.getName == "typeArgs" => m }.head
          val out = mm.invoke(m, tpe).asInstanceOf[List[TypeRepr]]
          out
      }

      val argInheritance = args.filterNot(termination.contains).flatMap {
        x =>
          termination.add(x)
          inspectToBToFull(x)
      }

      (main ++ argInheritance).distinct
    }

    // FIXME reimplement using `baseClasses`
    private def inspectSymbolToFull(symbol: Symbol): List[(AbstractReference, AbstractReference)] = {
      symbol.tree match {
        case c: ClassDef =>
          val trees = c.parents.collect { case tt: TypeTree => tt }
          if (trees.nonEmpty) log(s"Found parent trees for symbol ${symbol.tree.show}: $trees")

          val o = trees.flatMap(inspectTreeToFull)
          val selfRef = inspector.inspectSymbolTree(symbol)
          val p = trees.flatMap(t => List((selfRef, inspector.inspectTypeRepr(t.tpe))))
          (p ++ o).distinct

        case t: TypeDef =>
          log(s"FullDbInspector: Found TypeDef symbol ${t.show}")
          inspectTreeToFull(t.rhs.asInstanceOf[TypeTree])

        case o =>
          throw new RuntimeException(s"Shit tree: $o")
          List.empty
      }
    }

    private def inspectToBToFull(tpe: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      tpe.dealias match {
        case t: TypeBounds =>
          inspectTypeReprToFullBases(t.hi) ++ inspectTypeReprToFullBases(t.low)
        case t: TypeRepr =>
          inspectTypeReprToFullBases(t)
      }
    }
  }

}
