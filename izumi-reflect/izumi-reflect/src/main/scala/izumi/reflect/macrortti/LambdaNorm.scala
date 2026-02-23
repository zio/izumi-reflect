package izumi.reflect.macrortti

import izumi.reflect.macrortti.LightTypeTagRef._

private[macrortti] object LambdaNorm {

  def normalize(lambda: Lambda): Lambda = {
    normalizeRef(lambda, env = Nil) match {
      case l: Lambda =>
        l
      case other =>
        throw new IllegalStateException(s"Expected Lambda after normalization, got: $other")
    }
  }

  def maxFreeVarDepth(lambda: Lambda, resolveDepth: SymName.LambdaParamName => Int = _.depth): Int = {
    val bound = scala.collection.mutable.HashSet.empty[SymName.LambdaParamName]
    var maxDepth = -1

    def visitBoundaries(b: Boundaries): Unit = b match {
      case Boundaries.Defined(bottom, top) => visit(bottom); visit(top)
      case Boundaries.Empty =>
    }

    def checkFreeVar(lpn: SymName.LambdaParamName): Unit = {
      if (!bound.contains(lpn) && lpn.depth >= 0) maxDepth = maxDepth.max(resolveDepth(lpn))
    }

    def visit(ref: AbstractReference): Unit = ref match {
      case l: Lambda =>
        val added = l.input.filter(bound.add)
        visit(l.output)
        added.foreach(bound.remove)
      case NameReference(lpn: SymName.LambdaParamName, boundaries, prefix) =>
        checkFreeVar(lpn)
        visitBoundaries(boundaries)
        prefix.foreach(visit)
      case NameReference(_, boundaries, prefix) =>
        visitBoundaries(boundaries)
        prefix.foreach(visit)
      case FullReference(lpn: SymName.LambdaParamName, parameters, prefix) =>
        checkFreeVar(lpn)
        parameters.foreach(p => visit(p.ref))
        prefix.foreach(visit)
      case FullReference(_, parameters, prefix) =>
        parameters.foreach(p => visit(p.ref))
        prefix.foreach(visit)
      case IntersectionReference(refs) => refs.foreach(visit)
      case UnionReference(refs) => refs.foreach(visit)
      case WildcardReference(boundaries) => visitBoundaries(boundaries)
      case Refinement(base, decls) =>
        visit(base)
        decls.foreach {
          case RefinementDecl.Signature(_, in, out) => in.foreach(visit); visit(out)
          case RefinementDecl.TypeMember(_, ref) => visit(ref)
        }
    }

    visit(lambda)
    maxDepth
  }

  private type Env = List[Map[SymName.LambdaParamName, SymName.LambdaParamName]]

  private def resolveParamName(paramName: SymName.LambdaParamName, env: Env): SymName.LambdaParamName = {
    env.iterator.map(_.get(paramName)).collectFirst { case Some(mapped) => mapped }.getOrElse(paramName)
  }

  private def resolveParamNamePreferOuter(paramName: SymName.LambdaParamName, env: Env): SymName.LambdaParamName = {
    val matches = env.iterator.map(_.get(paramName)).collect { case Some(mapped) => mapped }.toList
    matches.lastOption.getOrElse(paramName)
  }

  private def normalizeSymName(symName: SymName, env: Env, preferOuter: Boolean = false): SymName = {
    symName match {
      case l: SymName.LambdaParamName =>
        if (preferOuter) resolveParamNamePreferOuter(l, env) else resolveParamName(l, env)
      case other =>
        other
    }
  }

  private def normalizeBoundaries(boundaries: Boundaries, env: Env): Boundaries = {
    boundaries match {
      case Boundaries.Defined(bottom, top) =>
        Boundaries.Defined(normalizeRef(bottom, env), normalizeRef(top, env))
      case Boundaries.Empty =>
        Boundaries.Empty
    }
  }

  private def normalizeRef(reference: AbstractReference, env: Env): AbstractReference = {
    reference match {
      case l: Lambda =>
        val localFreeVarOffset = maxFreeVarDepth(l, lpn => resolveParamName(lpn, env).depth)
        val normalizedDepth = localFreeVarOffset + 1
        val newArity = l.input.size
        val localMap = l
          .input.zipWithIndex.iterator.map {
            case (param, idx) =>
              val d = if (param.depth < 0) param.depth else normalizedDepth
              param -> param.copy(index = idx, depth = d, arity = newArity)
          }.toMap
        val normalizedInput = l.input.map(p => localMap.getOrElse(p, throw new IllegalStateException(s"Missing normalized param for $p")))
        val nextEnv = localMap :: env
        new Lambda(normalizedInput, normalizeRef(l.output, nextEnv))
      case IntersectionReference(refs) =>
        IntersectionReference(refs.map(normalizeRef(_, env)).map {
          case a: AppliedReferenceExceptIntersection => a
          case other => throw new IllegalStateException(s"Expected AppliedReferenceExceptIntersection, got: $other")
        })
      case UnionReference(refs) =>
        UnionReference(refs.map(normalizeRef(_, env)).map {
          case a: AppliedReferenceExceptUnion => a
          case other => throw new IllegalStateException(s"Expected AppliedReferenceExceptUnion, got: $other")
        })
      case WildcardReference(boundaries) =>
        WildcardReference(normalizeBoundaries(boundaries, env))
      case Refinement(base, decls) =>
        val normalizedBase = normalizeRef(base, env) match {
          case a: AppliedReference => a
          case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
        }
        val normalizedDecls = decls.map {
          case RefinementDecl.Signature(name, in, out) =>
            RefinementDecl.Signature(
              name,
              in.map(normalizeRef(_, env)).map {
                case a: AppliedReference => a
                case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
              },
              normalizeRef(out, env) match {
                case a: AppliedReference => a
                case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
              }
            ): RefinementDecl
          case RefinementDecl.TypeMember(name, ref) =>
            RefinementDecl.TypeMember(name, normalizeRef(ref, env)): RefinementDecl
        }
        Refinement(normalizedBase, normalizedDecls)
      case NameReference(ref, boundaries, prefix) =>
        NameReference(
          normalizeSymName(ref, env),
          normalizeBoundaries(boundaries, env),
          prefix.map(normalizeRef(_, env)).map {
            case a: AppliedReference => a
            case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
          }
        )
      case FullReference(symName, parameters, prefix) =>
        FullReference(
          normalizeSymName(symName, env, preferOuter = true),
          parameters.map(p => TypeParam(normalizeRef(p.ref, env), p.variance)),
          prefix.map(normalizeRef(_, env)).map {
            case a: AppliedReference => a
            case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
          }
        )
    }
  }

}
