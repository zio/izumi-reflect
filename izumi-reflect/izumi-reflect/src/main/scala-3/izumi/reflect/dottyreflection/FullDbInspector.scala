package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.{LightTypeTagInheritance, LightTypeTagRef}
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
          )
      }

      out
    }

    private def processSymbol(r: TypeRef | ParamRef, selfRef: AbstractReference, onlyIndirect: Boolean): List[(AbstractReference, AbstractReference)] = {
      r.typeSymbol match {
        case s if s.isClassDef =>
          extractBase(r, selfRef, onlyIndirect = onlyIndirect)

        case s if s.isTypeDef =>
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

      val resolvedTypeMembers = collectClassTypeMembers(tpe)

      basesTermination.addTerminatingClsSym(tpe)

      val memberBasesRefs = extractTypeMemberBases(resolvedTypeMembers)

      log(s"For `${tpe.show}` (onlyIndirect=$onlyIndirect) found base types ${baseTypes.map(_.show)}")

      val recursiveParentRefs = baseTypes.flatMap {
        case t if basesTermination.isTerminatingClsSym(t) => Nil
        case t => inspectTypeReprToFullBases(t, onlyIndirect = true)
      }

      val directBaseRefs = if (onlyIndirect) {
        Nil
      } else {
        val classParents = baseTypes.map {
          bt =>
            val parentRef = inspector.inspectTypeRepr(bt)
            (selfRef, parentRef)
        }
        val refinementParent = makeSelfRefinementEdge(selfRef, resolvedTypeMembers)
        classParents ::: refinementParent
      }

      val mainBasesRefs = recursiveParentRefs ::: directBaseRefs

      argBasesRefs ::: memberBasesRefs ::: mainBasesRefs
    }

    private def collectClassTypeMembers(tpe: TypeRepr): List[(Symbol, TypeRepr)] = {
      if (tpe.classSymbol.isEmpty) {
        Nil
      } else {
        tpe.baseClasses
          .filter(isNonStdlibClass)
          .flatMap(_.declaredTypes)
          .filter(isInspectableTypeMember)
          .distinctBy(_.name)
          .flatMap(resolveFirstOrderMember(tpe, _))
      }
    }

    private def resolveFirstOrderMember(tpe: TypeRepr, decl: Symbol): Option[(Symbol, TypeRepr)] = {
      val memberTpe = resolveMemberBoundsOrType(tpe, decl)
      if (isHigherKindedMember(memberTpe)) None else Some((decl, memberTpe))
    }

    private def isNonStdlibClass(sym: Symbol): Boolean = {
      sym.isClassDef && {
        val fullName = sym.fullName
        !fullName.startsWith("scala.") && !fullName.startsWith("java.")
      }
    }

    private def isInspectableTypeMember(decl: Symbol): Boolean = {
      val flags = decl.flags
      decl.isTypeDef && !decl.isClassDef && !decl.isTypeParam &&
      !flags.is(Flags.Private) && !flags.is(Flags.PrivateLocal) &&
      !flags.is(Flags.Protected) && !flags.is(Flags.Synthetic) && !flags.is(Flags.Artifact)
    }

    private def resolveMemberBoundsOrType(tpe: TypeRepr, decl: Symbol): TypeRepr = {
      tpe.memberType(decl)._dealiasSimplifiedFull match {
        case tb: TypeBounds => tb
        case tr: TypeRef if tr.typeSymbol == decl => tr._underlying._dealiasSimplifiedFull
        case other => other
      }
    }

    private def isHigherKindedMember(memberTpe: TypeRepr): Boolean = memberTpe match {
      case _: LambdaType => true
      case TypeBounds(_: LambdaType, _) => true
      case TypeBounds(_, _: LambdaType) => true
      case _ => memberTpe._takesTypeArgs
    }

    private def extractTypeMemberBases(members: List[(Symbol, TypeRepr)]): List[(AbstractReference, AbstractReference)] = {
      members
        .flatMap {
          case (_, TypeBounds(low, hi)) =>
            if (low =:= hi) List(hi) else List(low, hi)
          case (_, other) =>
            List(other)
        }
        .filterNot(isIgnoredMemberBound)
        .flatMap(inspectTypeBoundsToFull)
    }

    private def isIgnoredMemberBound(bound: TypeRepr): Boolean = {
      ignoredInIntersections(bound) || ignoredInUnions(bound) || basesTermination.isTerminatingClsSym(bound)
    }

    private def makeSelfRefinementEdge(
      selfRef: AbstractReference,
      members: List[(Symbol, TypeRepr)]
    ): List[(AbstractReference, AbstractReference)] = {
      (selfRef, members) match {
        case (appliedSelf: AppliedReference, _ :: _) =>
          val decls = members.map { case (decl, memberTpe) => toTypeMemberDecl(decl, memberTpe) }.toSet
          List(selfRef -> LightTypeTagRef.Refinement(appliedSelf, decls))
        case _ =>
          Nil
      }
    }

    private def toTypeMemberDecl(decl: Symbol, memberTpe: TypeRepr): RefinementDecl.TypeMember = {
      val name = decl.name
      val ref = memberTpe match {
        case tb: TypeBounds =>
          val low = inspector.inspectTypeRepr(tb.low)
          val hi = inspector.inspectTypeRepr(tb.hi)
          if (!decl.flags.is(Flags.Deferred) || low == hi) {
            hi
          } else if (low == LightTypeTagInheritance.tpeNothing && hi == LightTypeTagInheritance.tpeAny) {
            NameReference(SymName.SymTypeName(name), Boundaries.Empty, None)
          } else {
            NameReference(SymName.SymTypeName(name), Boundaries.Defined(low, hi), None)
          }
        case other =>
          inspector.inspectTypeRepr(other)
      }
      RefinementDecl.TypeMember(name, ref)
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
