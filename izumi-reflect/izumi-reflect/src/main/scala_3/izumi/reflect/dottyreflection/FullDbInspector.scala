package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.collection.mutable
import scala.quoted._

abstract class FullDbInspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.reflect._
  private lazy val inspector = new Inspector(0) { val qctx: FullDbInspector.this.qctx.type = FullDbInspector.this.qctx }
  // @formatter:on

  def buildFullDb[T <: AnyKind: Type]: Map[AbstractReference, Set[AbstractReference]] = {
    val tpe = implicitly[Type[T]]
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
    private val termination = mutable.HashSet[TypeRepr]()

    def inspectTreeToFull(uns: TypeTree): List[(AbstractReference, AbstractReference)] = {
      val symbol = uns.symbol
      val tpe2 = uns.tpe

      if (symbol.isNoSymbol)
        inspectTTypeToFullBases(tpe2).distinct
      else
        inspectSymbolToFull(symbol).distinct
    }

    private def inspectTTypeToFullBases(tpe: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      val selfRef = inspector.inspectTypeRepr(tpe)

      tpe match {
        case a: AppliedType =>
          val baseTypes = a.baseClasses.map(b => a.baseType(b)).filterNot(termination.contains)
          log(s"For `$tpe` found base types $baseTypes")

          val main = (selfRef, selfRef) +: baseTypes.map {
            bt =>
              val parentRef = inspector.inspectTypeRepr(bt)
              (selfRef, parentRef)
          }

          val args = a.args.filterNot(termination.contains).flatMap {
            x =>
              termination.add(x)
              inspectToBToFull(x)
          }
          (main ++ args).distinct

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
          inspectTTypeToFullBases(a.left) ++ inspectTTypeToFullBases(a.right)

        case o: OrType =>
          inspectTTypeToFullBases(o.left) ++ inspectTTypeToFullBases(o.right)

        case r: TypeRef =>
          inspectSymbolToFull(r.typeSymbol)

        case r: ParamRef =>
          inspectSymbolToFull(r.typeSymbol)

        case b: TypeBounds =>
          inspectToBToFull(b)

        case o =>
          log(s"FullDbInspector: UNSUPPORTED: $o")
          List.empty
      }
    }

    private def inspectSymbolToFull(symbol: Symbol): List[(AbstractReference, AbstractReference)] = {
      symbol.tree match {
        case c: ClassDef =>
//          val parentSymbols = c.parents.map(_.symbol).filterNot(_.isNoSymbol)
          val trees = c.parents.collect { case tt: TypeTree => tt }
          if (trees.nonEmpty) log(s"Found parent trees for symbol ${symbol.tree.show}: $trees")

          val o = trees.flatMap(inspectTreeToFull)
          val selfRef = inspector.inspectSymbolTree(symbol)
          val p = trees.flatMap(t => List((selfRef, inspector.inspectTypeTree(t))))
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
      tpe match {
        case t: TypeBounds =>
          inspectTTypeToFullBases(t.hi) ++ inspectTTypeToFullBases(t.low)
        case t: TypeRepr =>
          inspectTTypeToFullBases(t)
      }
    }
  }

}
