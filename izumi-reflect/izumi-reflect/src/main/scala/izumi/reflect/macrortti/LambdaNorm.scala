package izumi.reflect.macrortti

import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec

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

  @tailrec
  private def resolveParamName(paramName: SymName.LambdaParamName, env: Env): SymName.LambdaParamName = {
    env match {
      case head :: tail =>
        head.get(paramName) match {
          case Some(mapped) => mapped
          case None => resolveParamName(paramName, tail)
        }
      case Nil =>
        paramName
    }
  }

  private def resolveParamNamePreferOuter(paramName: SymName.LambdaParamName, env: Env): SymName.LambdaParamName = {
    @tailrec
    def loop(rem: Env, found: SymName.LambdaParamName): SymName.LambdaParamName = {
      rem match {
        case head :: tail =>
          head.get(paramName) match {
            case Some(mapped) => loop(tail, mapped)
            case None => loop(tail, found)
          }
        case Nil =>
          found
      }
    }
    loop(env, paramName)
  }

  private def normalizeSymName(symName: SymName, env: Env, preferOuter: Boolean = false): SymName = {
    symName match {
      case l: SymName.LambdaParamName =>
        val resolved = if (preferOuter) resolveParamNamePreferOuter(l, env) else resolveParamName(l, env)
        if (resolved == l) l else resolved
      case other =>
        other
    }
  }

  private def normalizeBoundaries(boundaries: Boundaries, env: Env): Boundaries = {
    boundaries match {
      case b @ Boundaries.Defined(bottom, top) =>
        val normBottom = normalizeRef(bottom, env)
        val normTop = normalizeRef(top, env)
        if ((normBottom eq bottom) && (normTop eq top)) b else Boundaries.Defined(normBottom, normTop)
      case Boundaries.Empty =>
        Boundaries.Empty
    }
  }

  private def normalizePrefix(prefix: Option[AppliedReference], env: Env): Option[AppliedReference] = {
    prefix match {
      case Some(p) =>
        val normP = normalizeRef(p, env) match {
          case a: AppliedReference => a
          case other => throw new IllegalStateException(s"Expected AppliedReference, got: $other")
        }
        if (normP eq p) prefix else Some(normP)
      case None =>
        None
    }
  }

  private def normalizeRef(reference: AbstractReference, env: Env): AbstractReference = {
    reference match {
      case l: Lambda =>
        val localFreeVarOffset = maxFreeVarDepth(l, lpn => resolveParamName(lpn, env).depth)
        val normalizedDepth = localFreeVarOffset + 1
        val newArity = l.input.size
        var inputChanged = false
        val localMap = l
          .input.zipWithIndex.iterator.map {
            case (param, idx) =>
              val d = if (param.depth < 0) param.depth else normalizedDepth
              val normParam = if (param.index == idx && param.depth == d && param.arity == newArity) {
                param
              } else {
                inputChanged = true
                param.copy(index = idx, depth = d, arity = newArity)
              }
              param -> normParam
          }.toMap
        val normalizedInput = if (inputChanged) {
          l.input.map(p => localMap.getOrElse(p, throw new IllegalStateException(s"Missing normalized param for $p")))
        } else {
          l.input
        }
        val nextEnv = localMap :: env
        val normalizedOutput = normalizeRef(l.output, nextEnv)
        if (!inputChanged && (normalizedOutput eq l.output)) {
          l
        } else {
          Lambda.unsafeDenormalized(normalizedInput, normalizedOutput)
        }
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
      case w @ WildcardReference(boundaries) =>
        val normBoundaries = normalizeBoundaries(boundaries, env)
        if (normBoundaries eq boundaries) w else WildcardReference(normBoundaries)
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
      case n @ NameReference(ref, boundaries, prefix) =>
        val normRef = normalizeSymName(ref, env)
        val normBoundaries = normalizeBoundaries(boundaries, env)
        val normPrefix = normalizePrefix(prefix, env)
        if ((normRef eq ref) && (normBoundaries eq boundaries) && (normPrefix eq prefix)) {
          n
        } else {
          NameReference(normRef, normBoundaries, normPrefix)
        }
      case f @ FullReference(symName, parameters, prefix) =>
        val normSym = normalizeSymName(symName, env, preferOuter = true)
        var paramsChanged = false
        val normParams = parameters.map {
          p =>
            val normRef = normalizeRef(p.ref, env)
            if (normRef eq p.ref) p
            else {
              paramsChanged = true
              TypeParam(normRef, p.variance)
            }
        }
        val normPrefix = normalizePrefix(prefix, env)
        if ((normSym eq symName) && !paramsChanged && (normPrefix eq prefix)) {
          f
        } else {
          FullReference(normSym, if (paramsChanged) normParams else parameters, normPrefix)
        }
    }
  }

}
