package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef.*

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.quoted.*

object FullDbInspector {
  def make(q: Quotes): FullDbInspector { val qctx: q.type } = new FullDbInspector(0) {
    override val qctx: q.type = q
  }
}

abstract class FullDbInspector(protected val shift: Int) extends InspectorBase {
  import qctx.reflect._

  private lazy val inspector0 = Inspector.make(qctx)

  def buildFullDb[T <: AnyKind: Type]: Map[AbstractReference, Set[AbstractReference]] = {
    new Run(inspector0)
      .inspectTypeReprToFullBases(TypeRepr.of[T])
      .distinct
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }

  class Run(i: Inspector { val qctx: FullDbInspector.this.qctx.type }) {
    private val termination = mutable.HashSet.empty[TypeRepr]

    def inspectTypeReprToFullBases(tpe0: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      val tpe = tpe0.dealias.simplified
      def selfRef: AbstractReference = i.inspectTypeRepr(tpe)

      tpe match {
        case a: AppliedType =>
          extractBase(a, selfRef, recurseIntoBases = false)

        case typeLambda: TypeLambda =>
          val selfL = i.inspectTypeRepr(typeLambda).asInstanceOf[LightTypeTagRef.Lambda]

          val parents = new Run(i.nextLam(typeLambda)).inspectTypeBoundsToFull(typeLambda.resType)

          val out = parents.flatMap {
            case (child0, parent0) =>
              val child = if (child0 == selfL.output) { // if child == typeLambda.resType, use typeLambda itself
                selfL
              } else {
                child0
              }

              // For Scala 2: see LightTypeTagImpl.makeLambdaOnlyBases.makeLambdaParents
              def lambdify(parentOrChild: LightTypeTagRef): AbstractReference = parentOrChild match {
                case l: Lambda =>
                  l
                case applied: AppliedReference =>
                  val l = LightTypeTagRef.Lambda(selfL.input, applied)
                  if (l.someArgumentsReferenced) l else applied
              }

              val childMaybeAsLambda = lambdify(child)
              val parentMaybeAsLambda = lambdify(parent0)

              Seq(
                (childMaybeAsLambda, parentMaybeAsLambda)
                // you may debug by inserting some debug trash into dbs:
//                NameReference(SymName.SymTypeName(s"LEFT ${System.nanoTime()} before:$child after:$childMaybeAsLambda")) ->
//                NameReference(SymName.SymTypeName(s"RIGHT before:$parent0 after:$parentMaybeAsLambda"))
              )
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
      val baseTypes = tpe
        .baseClasses
        .iterator.map(tpe.baseType)
        .filterNot(termination)
        .toList
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
          val parentRef = i.inspectTypeRepr(bt)
          (selfRef, parentRef)
      }

      val typeArgs: List[TypeRepr] = tpe match {
        case a: AppliedType =>
          a.args
        case _ =>
          tpe.typeArgs
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
