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
  import qctx.reflect._
  private lazy val inspector = new Inspector(0) { val qctx: DbInspector.this.qctx.type = DbInspector.this.qctx }
  // @formatter:on

  def buildNameDb[T <: AnyKind: Type]: Map[NameReference, Set[NameReference]] = {
    val tpeTree = TypeTree.of[T]
    new Run()
      .inspectTreeToName(tpeTree)
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
    private val oMap = mutable.HashMap[Symbol, List[(NameReference, NameReference)]]()
    private val pMap = mutable.HashMap[Symbol, List[(NameReference, NameReference)]]()

    def inspectTreeToName(typeTree: TypeTree): List[(NameReference, NameReference)] = {
      val symbol = typeTree.symbol
      val tpe2 = typeTree.tpe

      if (symbol.isNoSymbol)
        inspectTypeReprToNameBases(tpe2).distinct
      else
        inspectSymbolToName(symbol).distinct
    }

    private def inspectTypeReprToNameBases(tpe2: TypeRepr): List[(NameReference, NameReference)] = {
      tpe2 match {
        case a: AppliedType =>
          val main = a.baseClasses.flatMap(inspectSymbolToName) // (a.tycon)
          val args = a.args.filterNot(termination.contains).flatMap {
            x =>
              termination.add(x)
              inspectToBToName(x)
          }
          (main ++ args).distinct

        case l: TypeLambda =>
          inspectTypeReprToNameBases(l.resType)

        case a: AndType =>
          inspectTypeReprToNameBases(a.left) ++ inspectTypeReprToNameBases(a.right)

        case o: OrType =>
          inspectTypeReprToNameBases(o.left) ++ inspectTypeReprToNameBases(o.right)

        case r: TypeRef =>
          inspectSymbolToName(r.typeSymbol)

        case r: ParamRef =>
          inspectSymbolToName(r.typeSymbol)

        case b: TypeBounds =>
          inspectToBToName(b)

        case c: ConstantType =>
          inspectTypeReprToNameBases(c.widen)

        case o =>
          log(s"DbInspector: UNSUPPORTED: $o")
          List.empty
      }
    }

    private def inspectSymbolToName(symbol: Symbol): List[(NameReference, NameReference)] = {
      symbol.tree match {
        case c: ClassDef =>
          //val parentSymbols = c.parents.map(_.symbol).filterNot(_.isNoSymbol)

          val trees = c.parents.collect { case tt: TypeTree => tt }
          val o =  {
              oMap.get(symbol) match {
                case None =>
                  val res = trees.flatMap(inspectTreeToName)
                  oMap.put(symbol, res)
                  res
                case Some(res) =>
                  res
              }
          }
          val selfRef = inspector.makeNameReferenceFromSymbol(symbol)

          val p = {
            pMap.get(symbol) match {
              case None =>
                val res = trees.flatMap {
                  t =>
                    val tRef = inspector.inspectTypeRepr(t.tpe)

                    tRef match {
                      case n: NameReference =>
                        List((selfRef, n))
                      case n: FullReference =>
                        List((selfRef, n.asName))
                      case _ =>
                        List.empty
                    }
                }
                pMap.put(symbol, res)
                res
              case Some(res) =>
                res
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
          inspectTypeReprToNameBases(t.hi) ++ inspectTypeReprToNameBases(t.low)
        case t: TypeRepr =>
          inspectTypeReprToNameBases(t)
      }
    }
  }

}
