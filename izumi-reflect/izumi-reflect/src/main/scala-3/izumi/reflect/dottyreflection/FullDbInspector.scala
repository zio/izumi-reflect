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

  def buildFullDb(typeRepr: TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    new Run(Inspector.make(qctx), mutable.HashSet.empty, mutable.HashSet.empty)
      .inspectTypeReprToFullBases(typeRepr, onlyIndirect = false)
      .iterator
      .filterNot {
        case (t, parent) =>
          parent == t
      }
      .toMultimap
  }

  class Run(
    inspector: Inspector { val qctx: FullDbInspector.this.qctx.type },
    basesTermination: mutable.HashSet[Symbol],
    toLambdaTermination: mutable.HashSet[Symbol]
  ) {
    def inspectTypeReprToFullBases(tpe0: TypeRepr, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      val tpe = tpe0._dealiasSimplifiedFull
      def selfRef(): AbstractReference = inspector.inspectTypeRepr(tpe)

      tpe match {
        case appliedType: AppliedType =>
          extractBase(appliedType, selfRef(), onlyIndirect = onlyIndirect) ++ extractLambdaBase(appliedType, onlyIndirect = onlyIndirect)

        case typeLambda: TypeLambda =>
          val resultTypeParents = new Run(inspector.nextLam(typeLambda), basesTermination, toLambdaTermination).inspectTypeBoundsToFull(typeLambda.resType)

          makeLambdaParents(selfRef(), resultTypeParents)

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

    // Equivalent to Scala 2 LightTypeTagImpl.makeLambdaOnlyBases
    private def extractLambdaBase(appliedType: AppliedType, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      if (onlyIndirect || toLambdaTermination.isTerminatingClsSym(appliedType)) {
        Nil
      } else {
        appliedType._etaExpand match {
          case None => Nil
          case Some(typeLambda) =>
            toLambdaTermination.addTerminatingClsSym(appliedType)

            val resultTypeParents = new Run(inspector.nextLam(typeLambda), basesTermination, toLambdaTermination)
              .inspectTypeBoundsToFull(typeLambda.resType)

            makeLambdaParents(inspector.inspectTypeRepr(typeLambda), resultTypeParents)
        }
      }
    }

    private def makeLambdaParents(
      selfRef: AbstractReference,
      resultTypeParents: List[(AbstractReference, AbstractReference)]
    ): List[(AbstractReference, AbstractReference)] = {
      val selfL = selfRef.asInstanceOf[Lambda]

      val out = resultTypeParents.flatMap {
        case (child0, parent0) =>
          val child = if (child0 == selfL.output) { // if child == typeLambda.resType, use typeLambda itself
            selfL
          } else {
            child0
          }

          // For Scala 2: see LightTypeTagImpl.makeLambdaOnlyBases.makeLambdaParents
          def maybeToLambda(parentOrChild: LightTypeTagRef): AbstractReference = parentOrChild match {
            case l: Lambda =>
              l
            case applied: AppliedReference =>
              val l = LightTypeTagRef.Lambda(selfL.input, applied)
              if (l.someArgumentsReferenced) l else applied
          }

          val childMaybeAsLambda = maybeToLambda(child)
          val parentMaybeAsLambda = maybeToLambda(parent0)

          Seq(
            (childMaybeAsLambda, parentMaybeAsLambda)
            // you may debug by inserting some trash into the dbs:
            //                NameReference(SymName.SymTypeName(s"LEFT ${System.nanoTime()} before:$child after:$childMaybeAsLambda")) ->
            //                NameReference(SymName.SymTypeName(s"RIGHT before:$parent0 after:$parentMaybeAsLambda"))
          )
      }

      out
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
      val argBasesRefs = tpe.typeArgs.flatMap {
        case t if basesTermination.isTerminatingClsSym(t) => Nil
        case t =>
          inspectTypeBoundsToFull(t)
      }

      val baseTypes: List[TypeRepr] = tpe
        .baseClasses
        .map(tpe.baseType)
        .filterNot(_ =:= tpe)

      basesTermination.addTerminatingClsSym(tpe)

      log(s"For `${tpe.show}` (onlyIndirect=$onlyIndirect) found base types ${baseTypes.map(_.show)}")

      val recursiveParentRefs = baseTypes.flatMap {
        case t if basesTermination.isTerminatingClsSym(t) => Nil
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

      val typeMemberEntries = if (onlyIndirect) {
        Nil
      } else {
        makeTypeMemberRefinementEntries(tpe, selfRef)
      }

      argBasesRefs ::: mainBasesRefs ::: typeMemberEntries
    }

    private def makeTypeMemberRefinementEntries(tpe: TypeRepr, selfRef: AbstractReference): List[(AbstractReference, AbstractReference)] = {
      val clsSym = tpe.classSymbol match {
        case Some(sym) => sym
        case None => return Nil
      }

      val typeMembers = clsSym.declaredTypes.filter { s =>
        s.isTypeDef && !s.isTypeParam && !s.isClassDef
      }
      if (typeMembers.isEmpty) return Nil

      val decls: Set[RefinementDecl] = typeMembers.flatMap { decl =>
        val declName = decl.name
        decl._info match {
          case _: TypeLambda => None // skip higher-kinded type members
          case tb: TypeBounds =>
            val boundaries = inspector.inspectBoundsImpl(tb)
            val ref: AbstractReference = boundaries match {
              case Boundaries.Defined(low, high) if high == low =>
                high
              case definedBoundaries =>
                NameReference(SymName.SymTypeName(declName), definedBoundaries, None)
            }
            Some(RefinementDecl.TypeMember(declName, ref): RefinementDecl)
          case concrete =>
            val ref = inspector.inspectTypeRepr(concrete)
            Some(RefinementDecl.TypeMember(declName, ref): RefinementDecl)
        }
      }.toSet

      if (decls.isEmpty) return Nil

      selfRef match {
        case applied: LightTypeTagRef.AppliedReference =>
          List(applied -> LightTypeTagRef.Refinement(applied, decls))
        case _ =>
          Nil
      }
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

    extension (set: mutable.HashSet[Symbol]) {

      private def addTerminatingClsSym(typeRepr: TypeRepr): Unit = {
        typeRepr.classSymbol match {
          case Some(clsSym) => set.add(clsSym)
          case _ =>
        }
      }

      private def isTerminatingClsSym(t: TypeRepr): Boolean = {
        t.classSymbol match {
          case Some(clsSym) => set.contains(clsSym)
          case None => false
        }
      }

    }

  }

}
