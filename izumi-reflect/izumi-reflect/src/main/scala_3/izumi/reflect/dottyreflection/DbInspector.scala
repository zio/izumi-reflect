package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._
import scala.collection.mutable

import scala.quoted._


abstract class DbInspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.reflect.{given, _}
  private lazy val inspector = new Inspector(0) { val qctx: DbInspector.this.qctx.type = DbInspector.this.qctx }
  // @formatter:on


  def buildNameDb[T <: AnyKind : Type]: Map[NameReference, Set[NameReference]] = {
    val tpe = implicitly[Type[T]]
    val uns = tpe.unseal
    new Run().inspectTreeToName(uns)
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents
            .collect {
              case r: AppliedNamedReference =>
                r.asName
            }
            .filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }


  private class Run() {
    private val termination = mutable.HashSet[TypeRepr]()

    def inspectTreeToName(uns: TypeTree): List[(NameReference, NameReference)] = {
      val symbol = uns.symbol
      val tpe2 = uns.tpe

      if (symbol.isNoSymbol)
        inspectTTypeToNameBases(tpe2).distinct
      else
        inspectSymbolToName(symbol).distinct
    }

    private def inspectTTypeToNameBases(tpe2: TypeRepr): List[(NameReference, NameReference)] = {
      tpe2 match {
        case a: AppliedType =>
          val main = a.baseClasses.flatMap(inspectSymbolToName) // (a.tycon)
          val args = a.args.filterNot(termination.contains).flatMap { x =>
            termination.add(x)
            inspectToBToName(x)
          }
          (main ++ args).distinct

        case l: TypeLambda =>
          inspectTTypeToNameBases(l.resType)

        case a: AndType =>
          inspectTTypeToNameBases(a.left) ++ inspectTTypeToNameBases(a.right)

        case o: OrType =>
          inspectTTypeToNameBases(o.left) ++ inspectTTypeToNameBases(o.right)

        case r: TypeRef =>
          inspectSymbolToName(r.typeSymbol)

        case b: TypeBounds =>
          inspectToBToName(b)

        case o =>
          log(s"DbInspector: UNSUPPORTED: $o")
          List.empty
      }
    }

    private def inspectSymbolToName(symbol: Symbol): List[(NameReference, NameReference)] = {
      symbol.tree match {
        case c: ClassDef =>
          //val parentSymbols = c.parents.map(_.symbol).filterNot(_.isNoSymbol)

          val trees = c.parents.collect {
            case tt: TypeTree =>
              tt
          }
          val o = trees.flatMap(inspectTreeToName)
          val selfRef = inspector.asNameRefSym(symbol)

          val p = trees.flatMap { t =>
            val tRef = inspector.inspectTree(t)

            tRef match {
              case n: NameReference =>
                List((selfRef, n))
              case n: FullReference =>
                List((selfRef, n.asName))
              case _ =>
                List.empty
            }
          }

          (p ++ o).distinct

        case t: TypeDef =>
          inspectTreeToName(t.rhs.asInstanceOf[TypeTree])
        case o =>
          List.empty
      }
    }

    private def inspectToBToName(tpe: TypeRepr): List[(NameReference, NameReference)] = {
      tpe match {
        case t: TypeBounds =>
          inspectTTypeToNameBases(t.hi) ++ inspectTTypeToNameBases(t.low)
        case t: TypeRepr =>
          inspectTTypeToNameBases(t)
      }
    }
  }

}

