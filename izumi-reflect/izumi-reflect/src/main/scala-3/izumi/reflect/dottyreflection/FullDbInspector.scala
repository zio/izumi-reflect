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
      .inspectTypeReprToFullBases(TypeRepr.of[T], onlyIndirect = false)
      .iterator
      .filterNot {
        case (t, parent) =>
          parent == t
      }
      .toMultimap
  }

  class Run(inspector: Inspector { val qctx: FullDbInspector.this.qctx.type }) {
    private val termination = mutable.HashSet.empty[Symbol]

    def inspectTypeReprToFullBases(tpe0: TypeRepr, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      val tpe = tpe0._dealiasSimplifiedFull
      def selfRef(): AbstractReference = inspector.inspectTypeRepr(tpe)

      tpe match {
        case appliedType: AppliedType =>
          extractBase(appliedType, selfRef(), onlyIndirect = onlyIndirect)

        case typeLambda: TypeLambda =>
          val selfL = selfRef().asInstanceOf[LightTypeTagRef.Lambda]

          val parents = new Run(inspector.nextLam(typeLambda)).inspectTypeBoundsToFull(typeLambda.resType)

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
                // you may debug by inserting some trash into the dbs:
//                NameReference(SymName.SymTypeName(s"LEFT ${System.nanoTime()} before:$child after:$childMaybeAsLambda")) ->
//                NameReference(SymName.SymTypeName(s"RIGHT before:$parent0 after:$parentMaybeAsLambda"))
              )
          }
          out

        case a: AndType =>
          inspectTypeReprToFullBases(a.left, onlyIndirect = false) ++ inspectTypeReprToFullBases(a.right, onlyIndirect = false)

        case o: OrType =>
          inspectTypeReprToFullBases(o.left, onlyIndirect = false) ++ inspectTypeReprToFullBases(o.right, onlyIndirect = false)

        case typeRef: TypeRef =>
          processSymbol(typeRef, selfRef(), onlyIndirect = onlyIndirect)

        case _: ParamRef =>
          // do not process type parameters for bases db
          Nil

        case termRef: TermRef =>
          extractBase(termRef, selfRef(), onlyIndirect = onlyIndirect)

        case b: TypeBounds =>
          processTypeBounds(b)

        case c: ConstantType =>
          extractBase(c, selfRef(), onlyIndirect = onlyIndirect)

        case t: ThisType =>
          inspectTypeReprToFullBases(t.tref, onlyIndirect = onlyIndirect)

        case r: Refinement =>
          refinementInfoToParts(r.info).flatMap(inspectTypeBoundsToFull)
            ++ inspectTypeReprToFullBases(r.parent, onlyIndirect = onlyIndirect)

        case other =>
          log(s"FullDbInspector: UNSUPPORTED: $other")
          extractBase(other, selfRef(), onlyIndirect = onlyIndirect)
      }
    }

    private def processSymbol(r: TypeRef | ParamRef, selfRef: AbstractReference, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      r.typeSymbol match {
        case s if s.isClassDef =>
          extractBase(r, selfRef, onlyIndirect = onlyIndirect)

        case s if s.isTypeDef =>
//          println(r -> s -> r._underlying)
          processTypeMemberWithTypeLambdaBounds(r, onlyIndirect = onlyIndirect)

        case o =>
          throw new RuntimeException(s"Unknown tree: ${o.getClass} $o $r ${o.tree} (pretty: ${o.tree.show})")
      }
    }

    private def extractBase(tpe: TypeRepr, selfRef: AbstractReference, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      addTerminatingClsSym(tpe)

      val baseTypes: List[TypeRepr] = tpe
        .baseClasses
        .map(tpe.baseType)
        .filterNot(_ =:= tpe)

      log(s"For `${tpe.show}` (onlyIndirect=$onlyIndirect) found base types ${baseTypes.map(_.show)}")

      val recursiveParentRefs = baseTypes.flatMap {
        case t if isTerminatingClsSym(t) => Nil
        case t => inspectTypeReprToFullBases(t, onlyIndirect = true)
      }

      val directBaseRefs = if (onlyIndirect) {
        Nil
      } else {
        baseTypes.map {
          bt =>
            val parentRef = inspector.inspectTypeRepr(bt)
            (selfRef, parentRef)
        }
      }

      val mainBasesRefs = recursiveParentRefs ::: directBaseRefs

      val argBasesRefs = tpe.typeArgs.flatMap {
        case t if isTerminatingClsSym(t) => Nil
        case t =>
          inspectTypeBoundsToFull(t)
      }

      mainBasesRefs ::: argBasesRefs
    }

    private def inspectTypeBoundsToFull(tpe: TypeRepr): List[(AbstractReference, AbstractReference)] = {
      tpe._dealiasSimplifiedFull match {
        case t: TypeBounds =>
          processTypeBounds(t)
        case t: TypeRepr =>
          inspectTypeReprToFullBases(t, onlyIndirect = false)
      }
    }

    private def processTypeBounds(tb: TypeBounds): List[(AbstractReference, AbstractReference)] = {
      inspectTypeReprToFullBases(tb.hi, onlyIndirect = false) ++ inspectTypeReprToFullBases(tb.low, onlyIndirect = false)
    }

    private def processTypeMemberWithTypeLambdaBounds(t: TypeRef | ParamRef, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      t._underlying match {
        // handle abstract higher-kinded type members specially,
        // move their upper bound into inheritance db, because they
        // will lose it after application. (Unlike proper type members)
        case TypeBounds(_, tl0: TypeLambda) =>
          val selfRef = inspector.inspectTypeRepr(t)
          // include only upper bound: we discard the lower bound for abstract higher-kinded type members
          val tl = tl0._dealiasSimplifiedFull
          val hiTypeLambda = inspector.inspectTypeRepr(tl)

          (selfRef, hiTypeLambda) :: replaceUpperBoundWithSelfInUpperBoundBases(selfRef, hiTypeLambda, tl, onlyIndirect = onlyIndirect)

        case underlying @ TypeBounds(_, _) =>
          val selfRef = inspector.inspectTypeRepr(t)
          extractBase(underlying, selfRef, onlyIndirect = onlyIndirect)

        // for opaque types
        case underlying =>
          inspectTypeReprToFullBases(underlying, onlyIndirect = onlyIndirect)
      }
    }

    private def replaceUpperBoundWithSelfInUpperBoundBases(
      selfRef: AbstractReference,
      upperBound: AbstractReference,
      upperBoundTpe: TypeRepr,
      onlyIndirect: Boolean
    ): List[(AbstractReference, AbstractReference)] = {
      val basesOfUpperBound = inspectTypeReprToFullBases(upperBoundTpe, onlyIndirect = onlyIndirect)
      basesOfUpperBound.map {
        case (k, v) if k == upperBound =>
          // bases of upper bound are also bases of the abstract type
          selfRef -> v
        case kv =>
          kv
      }
    }

    private def addTerminatingClsSym(typeRepr: TypeRepr): AnyVal = {
      typeRepr.classSymbol match {
        case Some(clsSym) => termination.add(clsSym)
        case _ =>
      }
    }

    private def isTerminatingClsSym(t: TypeRepr): Boolean = {
      t.classSymbol match {
        case Some(clsSym) => termination.contains(clsSym)
        case None => false
      }
    }

  }

}
