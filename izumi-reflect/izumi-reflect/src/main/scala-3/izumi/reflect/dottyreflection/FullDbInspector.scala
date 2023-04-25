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
          val selfL = selfRef.asInstanceOf[LightTypeTagRef.Lambda]

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
          // do not process type parameters for bases db
          Nil

        case termRef: TermRef =>
          extractBase(termRef, selfRef, false)

        case b: TypeBounds =>
          processTypeBounds(b)

        case c: ConstantType =>
          extractBase(c, selfRef, false)

        case t: ThisType =>
          inspectTypeReprToFullBases(t.tref)

        case r: Refinement =>
          refinementInfoToParts(r.info).flatMap(inspectTypeBoundsToFull)
          ++ inspectTypeReprToFullBases(r.parent)

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
          processTypeMemberWithTypeLambdaBounds(r)

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
          processTypeBounds(t)
        case t: TypeRepr =>
          inspectTypeReprToFullBases(t)
      }
    }

    private def processTypeBounds(tb: TypeBounds): List[(AbstractReference, AbstractReference)] = {
      inspectTypeReprToFullBases(tb.hi) ++ inspectTypeReprToFullBases(tb.low)
    }

    private def processTypeMemberWithTypeLambdaBounds(t: TypeRef | ParamRef): List[(AbstractReference, AbstractReference)] = {
      def replaceUpperBoundWithSelfInUpperBoundBases(selfRef: AbstractReference, upperBound: AbstractReference, upperBoundTpe: TypeRepr)
        : List[(AbstractReference, AbstractReference)] = {
        val basesOfUpperBound = inspectTypeReprToFullBases(upperBoundTpe)
        basesOfUpperBound.map {
          case (k, v) if k == upperBound =>
            // bases of upper bound are also bases of the abstract type
            selfRef -> v
          case kv =>
            kv
        }
      }

      val underlying = t._underlying
      underlying match {
        // handle abstract higher-kinded type members specially,
        // move their upper bound into inheritance db, because they
        // will lose it after application. (Unlike proper type members)
        case TypeBounds(_, tl0: TypeLambda) =>
          val selfRef = i.inspectTypeRepr(t)
          // include only upper bound: we discard the lower bound for abstract higher-kinded type members
          val tl = tl0.dealias.simplified
          val hiTypeLambda = i.inspectTypeRepr(tl)

          (selfRef, hiTypeLambda) :: replaceUpperBoundWithSelfInUpperBoundBases(selfRef, hiTypeLambda, tl)

        // for abstract proper type members, we do not include the upper bound itself into db
        // (because it's already in the type bound and unlike for type lambda members, the type bound is not lost.
        case TypeBounds(_, hi0) =>
          val selfRef = i.inspectTypeRepr(t)
          val hi = hi0.dealias.simplified
          val upperBound = i.inspectTypeRepr(hi)

          replaceUpperBoundWithSelfInUpperBoundBases(selfRef, upperBound, hi)

        case _ =>
          inspectTypeReprToFullBases(underlying)
      }
    }

  }

}
