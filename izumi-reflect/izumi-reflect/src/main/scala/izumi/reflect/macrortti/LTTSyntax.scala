package izumi.reflect.macrortti

import izumi.reflect.macrortti.LightTypeTagRef.SymName.LambdaParamName
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec

private[macrortti] trait LTTSyntax {
  this: LightTypeTagRef =>

  protected[this] final def combineImpl(args: Seq[LightTypeTagRef]): AbstractReference = {
    if (args.nonEmpty) {
      applySeq(args.map { case v: AbstractReference => v })
    } else {
      // while user is not expected to combine an arbitrary tag with an empty args list
      // it's a sound operation which should just return the tag itself
      // see also: https://github.com/7mind/izumi/pull/1528
      this match {
        case ref: AbstractReference =>
          ref
      }
    }
  }

  protected[this] final def combineNonPosImpl(args: Seq[Option[LightTypeTagRef]]): AbstractReference = {
    applyParameters {
      l =>
        l.input.zip(args).flatMap {
          case (p, v) =>
            v match {
              case Some(value: AbstractReference) =>
                Seq(p -> value)
              case None =>
                Seq.empty
            }
        }
    }
  }

  protected[this] final def withoutArgsImpl: AbstractReference = {
    def appliedNamedReference(reference: AppliedNamedReference) = {
      reference match {
        case LightTypeTagRef.NameReference(_, _, _) => reference
        case r @ LightTypeTagRef.FullReference(_, parameters @ _, prefix) => NameReference(r.symName, Boundaries.Empty, prefix)
      }
    }

    def appliedReference(reference: AppliedReference): AppliedReference = {
      reference match {
        case reference: AppliedNamedReference =>
          appliedNamedReference(reference)
        case LightTypeTagRef.IntersectionReference(refs) =>
          LightTypeTagRef.maybeIntersection(refs.map(appliedReference))
        case LightTypeTagRef.UnionReference(refs) =>
          LightTypeTagRef.maybeUnion(refs.map(appliedReference))
        case LightTypeTagRef.Refinement(reference, decls) =>
          LightTypeTagRef.Refinement(appliedReference(reference), decls)
        case r: LightTypeTagRef.WildcardReference =>
          r
      }
    }

    @tailrec
    def go(self: LightTypeTagRef): AbstractReference = {
      self match {
        case Lambda(_, output) =>
          go(output)
        case reference: AppliedReference =>
          appliedReference(reference)
      }
    }

    go(this)
  }

  /** Render to string, omitting package names */
  protected[this] final def toStringImpl: String = {
    import izumi.reflect.macrortti.LTTRenderables.Short._
    (this: LightTypeTagRef).render()
  }

  /** Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  protected[this] final def reprImpl: String = {
    import izumi.reflect.macrortti.LTTRenderables.Long._
    (this: LightTypeTagRef).render()
  }

  protected[this] final def shortNameImpl: String = {
    getName(r => LTTRenderables.Short.r_SymName(r.symName, hasPrefix = false))
  }

  protected[this] final def longNameWithPrefixImpl: String = {
    getName(r => LTTRenderables.LongPrefixDot.r_NameRefRenderer.render(NameReference(r.symName, Boundaries.Empty, r.prefix)))
  }

  protected[this] final def longNameInternalSymbolImpl: String = {
    getName(r => LTTRenderables.Long.r_SymName(r.symName, hasPrefix = false))
  }

  protected[this] final def scalaStyledNameImpl: String = {
    import izumi.reflect.macrortti.LTTRenderables.ScalaStyledLambdas._
    (this: LightTypeTagRef).render()
  }

  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  protected[this] final def longNameImpl: String = {
    longNameInternalSymbol
  }

  protected[this] final def getPrefixImpl: Option[LightTypeTagRef] = {
    @tailrec
    @inline
    def getPrefix(self: LightTypeTagRef): Option[LightTypeTagRef] = {
      self match {
        case Lambda(_, output) => getPrefix(output)
        case NameReference(_, _, prefix) => prefix
        case FullReference(_, _, prefix) => prefix
        case IntersectionReference(refs) =>
          val prefixes = refs.flatMap(_.getPrefix).collect {
            case p: AppliedReference => p
          }
          if (prefixes.nonEmpty) Some(maybeIntersection(prefixes)) else None
        case UnionReference(refs) =>
          val prefixes = refs.flatMap(_.getPrefix).collect {
            case p: AppliedReference => p
          }
          if (prefixes.nonEmpty) Some(maybeUnion(prefixes)) else None
        case Refinement(reference, _) => getPrefix(reference)
        case _: WildcardReference => None
      }
    }

    getPrefix(this)
  }

  protected[this] final def typeArgsImpl: List[AbstractReference] = {
    this match {
      case Lambda(input, output) =>
        val params = input.iterator.toSet[SymName]
        output.typeArgs.filter {
          case n: AppliedNamedReference =>
            !params.contains(n.asName.ref)
          case _ =>
            true
        }
      case NameReference(_, _, _) =>
        Nil
      case FullReference(_, parameters, _) =>
        parameters.map(_.ref)
      case IntersectionReference(_) =>
        Nil
      case UnionReference(_) =>
        Nil
      case WildcardReference(_) =>
        Nil
      case Refinement(reference, _) =>
        reference.typeArgs
    }
  }

  /** decompose intersection type */
  protected[this] final def decomposeImpl: Set[AppliedReference] = {
    this match {
      case IntersectionReference(refs) =>
        refs.flatMap(_.decompose)
      case appliedReference: AppliedReference =>
        Set(appliedReference)
      // lambdas cannot appear _inside_ intersections
      case Lambda(_, _) =>
        Set.empty
    }
  }

  protected[this] final def decomposeUnionImpl: Set[AppliedReference] = {
    this match {
      case UnionReference(refs) =>
        refs.flatMap(_.decompose)
      case appliedReference: AppliedReference =>
        Set(appliedReference)
      // lambdas cannot appear _inside_ unions
      case Lambda(_, _) =>
        Set.empty
    }
  }

  private[macrortti] final def applySeq(refs: Seq[AbstractReference]): AbstractReference = {
    applyParameters {
      l =>
        l.input.zip(refs).map {
          case (p, v) =>
            p -> v
        }
    }
  }

  private[macrortti] final def applyParameters(p: Lambda => Seq[(LambdaParamName, AbstractReference)]): AbstractReference = {
    this match {
      case l: Lambda =>
        val parameters = p(l)
        if (l.input.size < parameters.size) {
          throw new IllegalArgumentException(s"$this expects no more than ${l.input.size} parameters: ${l.input} but got $parameters")
        }
        val expected = l.input.iterator.toSet
        val unknownKeys = parameters.iterator.map(_._1).toSet.diff(expected)
        if (unknownKeys.nonEmpty) {
          throw new IllegalArgumentException(s"$this takes parameters: $expected but got unexpected ones: $unknownKeys")
        }

        RuntimeAPI.applyLambda(l, parameters)
      case _ =>
        throw new IllegalArgumentException(s"$this is not a type lambda, it cannot be parameterized")
    }
  }

  @inline
  private[this] final def getName(render: AppliedNamedReference => String): String = {
    @tailrec
    @inline
    def go(r: LightTypeTagRef): String = r match {
      case Lambda(_, output) => go(output)
      case ref: NameReference => render(ref)
      case ref: FullReference => render(ref)
      case IntersectionReference(refs) => refs.map(goDeep).mkString(" & ")
      case UnionReference(refs) => refs.map(goDeep).mkString(" | ")
      case Refinement(reference, _) => go(reference)
      case WildcardReference(_) => "?"
    }

    def goDeep(r: LightTypeTagRef): String = go(r)

    go(this)
  }
}
