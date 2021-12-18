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
    val tpe = TypeRepr.of[T]
    new Run()
      .inspectTypeReprToFullBases(tpe)
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
      val tpe = tpe0._fullNormDealiasSimplified

      tpe match {
        case t if ignored(t) =>
          Nil

        case a: AppliedType =>
          val main = inspectType(a)
          val args = a.args.filterNot(termination.contains).flatMap {
            arg =>
              termination.add(arg)
              inspectToBToFull(arg)
          }
          (main ++ args).distinct

        case l: TypeLambda =>
          val parents = inspectToBToFull(l.resType)
          val selfL = inspector.inspectTypeRepr(l).asInstanceOf[LightTypeTagRef.Lambda]
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
          // inspectTypeReprToFullBases(r.qualifier.memberType(r.typeSymbol))
          // FIXME below still required for non `subtype check fails when child type has absorbed a covariant type parameter of the supertype`
          inspectType(r)

        case r: ParamRef =>
          inspectType(r)

        case b: TypeBounds =>
          inspectToBToFull(b)

        case c: ConstantType =>
          inspectTypeReprToFullBases(c.widen)

        case o =>
          log(s"FullDbInspector: UNSUPPORTED: $o")
          Nil
      }
    }

    // FIXME reimplement using `baseClasses`
    private def inspectType(tpe: TypeRepr): List[(AbstractReference, AbstractReference)] = {

      def impl(a: TypeRepr) = {
        val selfRef = inspector.inspectTypeRepr(a)
        val bases = a.baseClasses.iterator.map(a.baseType).filterNot(_ == tpe).filterNot(termination).toList

        log(s"For `$tpe` ($selfRef) found base types $bases")

        val parentRefs = bases.map {
          parent =>
            val parentRef = inspector.inspectTypeRepr(parent)
            (selfRef, parentRef)
        }
        // previous behavior: this seems to contradict Scala 2 version
//        val parentBases = bases.flatMap(inspectTypeReprToFullBases)
//        parentRefs ++ parentBases
        parentRefs
      }
      impl(tpe)

//      val headBase :: bases0 = tpe.baseClasses
//      val bases = if (headBase != tpe.typeSymbol) headBase :: bases0 else bases0 // literals and TermRefs should include the first class of baseClasses
//      if (bases.nonEmpty) {
//        log(s"Found bases for type ${tpe.show}($tpe): $bases")
//      }
//
//      val selfRef = inspector.inspectTypeRepr(tpe)
//      val selfBases = bases.flatMap(t => List((selfRef, inspector.inspectTypeRepr(t.tpe))))
//      val parentBases = bases.flatMap(inspectTypeReprToFullBases apply _.tpe)
//      selfBases ++ parentBases

//      symbol.tree match {
//        case c: ClassDef =>
//          val extendsRefs = c.parents.collect { case tt: TypeTree => tt }
//          if (extendsRefs.nonEmpty) log(s"Found parent trees for symbol ${symbol.tree.show}: $extendsRefs")
//
//          val selfRef = inspector.inspectTypeRepr(symbolTpe)
//          val selfBases = extendsRefs.flatMap(t => List((selfRef, inspector.inspectTypeRepr(t.tpe))))
//          val parentBases = extendsRefs.flatMap(inspectTypeReprToFullBases apply _.tpe)
//          selfBases ++ parentBases
//
//        case t: TypeDef =>
//          log(s"FullDbInspector: Found TypeDef symbol ${t.show}")
//          inspectTypeReprToFullBases(t.rhs.asInstanceOf[TypeTree].tpe)
//
//        case o =>
//          throw new RuntimeException(s"Shit tree: $o")
//          List.empty
//      }
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
