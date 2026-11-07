/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect.macrortti

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, AppliedReference}
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{LambdaParamName, SymTypeName}

import scala.runtime.AbstractFunction3
import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

sealed trait LightTypeTagRef extends LTTSyntax with Serializable {

  // bincompat with versions before 2.3.0
  final def combine(args: Seq[LightTypeTagRef]): AbstractReference = this.combineImpl(args)
  final def combineNonPos(args: Seq[Option[LightTypeTagRef]]): AbstractReference = this.combineNonPosImpl(args)
  final def withoutArgs: AbstractReference = this.withoutArgsImpl
  /** Render to string, omitting package names */
  override final def toString: String = this.toStringImpl
  /** Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  final def repr: String = this.reprImpl
  final def scalaStyledRepr: String = this.scalaStyledReprImpl
  @deprecated("Renamed to scalaStyledRepr", "3.0.8")
  final def scalaStyledName: String = this.scalaStyledReprImpl
  final def shortName: String = this.shortNameImpl
  final def longNameWithPrefix: String = this.longNameWithPrefixImpl
  final def longNameInternalSymbol: String = this.longNameInternalSymbolImpl
  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  final def longName: String = this.longNameInternalSymbolImpl
  final def getPrefix: Option[AppliedReference] = this.getPrefixImpl
  final def typeArgs: List[AbstractReference] = this.typeArgsImpl
  /** decompose intersection type */
  final def decompose: Set[LightTypeTagRef.AppliedReferenceExceptIntersection] = this.decomposeImpl
  final def decomposeUnion: Set[LightTypeTagRef.AppliedReferenceExceptUnion] = this.decomposeUnionImpl

}

object LightTypeTagRef extends LTTOrdering {
  import LTTRenderables.Short._
//  import LTTRenderables.Long._

  sealed trait AbstractReference extends LightTypeTagRef

  // bincompat only
  private[macrortti] sealed abstract class LambdaParameter {
    @deprecated("bincompat only", "20.02.2023")
    private[macrortti] def name: String
  }
  @deprecated("bincompat only", "20.02.2023")
  private[macrortti] object LambdaParameter extends (String => LambdaParameter) {
    @deprecated("bincompat only", "20.02.2023")
    def apply(name: String): LambdaParameter = {
      SymName.bincompatForceCreateLambdaParamNameFromString(name)
    }
  }

  final case class Lambda(input: List[SymName.LambdaParamName], output: AbstractReference) extends AbstractReference {
    override def hashCode(): Int = {
      normalizedOutput.hashCode()
    }

    lazy val inputSize: Int = input.size
    lazy val paramRefs: Set[NameReference] = input
      .iterator.map {
        n =>
          // No boundary on paramRefs
          // FIXME LambdaParameter should contain bounds and NameReference shouldn't
          //       (Except possibly lower bound of an abstract/opaque type member)
          NameReference(n)
      }.toSet
    lazy val referenced: Set[NameReference] = RuntimeAPI.unpack(this)
    def allArgumentsReferenced: Boolean = paramRefs.diff(referenced).isEmpty
    lazy val someArgumentsReferenced: Boolean = {
      val unusedParamsSize = paramRefs.diff(referenced).size
      unusedParamsSize < paramRefs.size
    }

    // Normalizes parameter indices and de Bruijn depths so nested lambdas compare structurally without fake placeholders.
    lazy val normalized: Lambda = LightTypeTagRef.normalizeLambda(this)
    lazy val normalizedParams: List[NameReference] = normalized.input.map(NameReference(_))
    lazy val normalizedOutput: AbstractReference = normalized.output

    override def equals(obj: Any): Boolean = {
      obj match {
        case l: Lambda =>
          inputSize == l.inputSize &&
          (normalizedOutput == l.normalizedOutput)

        case _ =>
          false
      }
    }
  }

  object LambdaConstants {
    final val defaultContextId = -1
    final val lambdaFakeParamDepth: Int = -2 // depth is always positive, unless fake
    final val tagMacro = -3
  }

  sealed trait AppliedReference extends AbstractReference

  sealed trait AppliedReferenceExceptIntersection extends AppliedReference
  sealed trait AppliedReferenceExceptUnion extends AppliedReference

  final case class IntersectionReference(refs: Set[AppliedReferenceExceptIntersection]) extends AppliedReferenceExceptUnion {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  final case class WildcardReference(boundaries: Boundaries) extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion

  final case class UnionReference(refs: Set[AppliedReferenceExceptUnion]) extends AppliedReferenceExceptIntersection {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  final case class Refinement(reference: AppliedReference, decls: Set[RefinementDecl]) extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  def isIgnored[T <: AbstractReference](t: T): Boolean = {
    t match {
      case a: AppliedReference =>
        ignored.contains(a)
      case _: Lambda =>
        false
    }
  }

  private[reflect] def ignored[T >: NameReference]: Set[T] = ignored0.asInstanceOf[Set[T]]
  private[this] val ignored0: Set[NameReference] = Set[NameReference](
    LightTypeTagInheritance.tpeAny,
    LightTypeTagInheritance.tpeMatchable,
    LightTypeTagInheritance.tpeAnyRef,
    LightTypeTagInheritance.tpeObject
  )

  def maybeIntersection(refs0: Iterator[_ <: LightTypeTagRef]): AppliedReference = {
    val refs = refs0.flatMap(_.decompose).toSet // flatten nested intersections
    if (refs.size == 1) {
      refs.head
    } else {
      val normalized = refs.diff(ignored)
      if (normalized.isEmpty) {
        LightTypeTagInheritance.tpeAny
      } else if (normalized.size == 1) {
        normalized.head
      } else {
        IntersectionReference(normalized)
      }
    }
  }

  private[this] val unionSuperTypePriority: List[NameReference] = List(
    LightTypeTagInheritance.tpeAny,
    LightTypeTagInheritance.tpeMatchable,
    LightTypeTagInheritance.tpeAnyRef,
    LightTypeTagInheritance.tpeObject
  )

  private[this] def pickDominantSuperType(normalized: Set[AppliedReferenceExceptUnion], superTypes: Set[AppliedReferenceExceptUnion]): AppliedReference = {
    unionSuperTypePriority.find(normalized.contains).getOrElse(superTypes.head)
  }

  def maybeUnion(refs0: Iterator[_ <: LightTypeTagRef]): AppliedReference = {
    val refs = refs0.flatMap(_.decomposeUnion).toSet // flatten nested unions
    val normalized = refs -- Set(LightTypeTagInheritance.tpeNothing, LightTypeTagInheritance.tpeNull)
    val superTypes = normalized.intersect(ignored)
    if (superTypes.nonEmpty) {
      pickDominantSuperType(normalized, superTypes)
    } else if (normalized.isEmpty) {
      LightTypeTagInheritance.tpeNothing
    } else if (normalized.size == 1) {
      normalized.head
    } else {
      UnionReference(normalized)
    }
  }

  private[this] final case class LambdaNormContext(
    currentDepth: Int,
    bound: Map[LambdaParamName, LambdaParamName],
    baseDepth: Int
  ) {
    def enterNested(rawInput: List[LambdaParamName], normInput: List[LambdaParamName]): LambdaNormContext = {
      LambdaNormContext(currentDepth + 1, bound ++ rawInput.zip(normInput), baseDepth)
    }
  }

  // Rewrites lambda parameter depths relative to the root lambda (0 for root, +k for nested, -k for enclosing outer scopes).
  private[reflect] def normalizeLambda(root: Lambda): Lambda = {
    val baseDepth = root.input.headOption.map(_.depth).filter(_ > 0).getOrElse(0)
    val arity = root.input.size
    val normInput = root.input.zipWithIndex.map {
      case (_, idx) => LambdaParamName(idx, 0, arity)
    }
    val ctx = LambdaNormContext(0, root.input.zip(normInput).toMap, baseDepth)
    Lambda(normInput, normalizeRef(root.output, ctx))
  }

  private[this] def normalizeNestedLambda(lambda: Lambda, parentCtx: LambdaNormContext): Lambda = {
    val nextDepth = parentCtx.currentDepth + 1
    val arity = lambda.input.size
    val normInput = lambda.input.zipWithIndex.map {
      case (_, idx) => LambdaParamName(idx, nextDepth, arity)
    }
    val nextCtx = parentCtx.enterNested(lambda.input, normInput)
    Lambda(normInput, normalizeRef(lambda.output, nextCtx))
  }

  private[this] def normalizeSym(sym: SymName, ctx: LambdaNormContext): SymName = sym match {
    case param: LambdaParamName =>
      ctx.bound.getOrElse(param, normalizeFreeParam(param, ctx))
    case other =>
      other
  }

  private[this] def normalizeFreeParam(param: LambdaParamName, ctx: LambdaNormContext): LambdaParamName = {
    if (param.depth == LambdaConstants.defaultContextId && ctx.baseDepth > 0) {
      LambdaParamName(param.index, -1, param.arity)
    } else if (param.depth >= 0 && param.depth < ctx.baseDepth) {
      LambdaParamName(param.index, param.depth - ctx.baseDepth, param.arity)
    } else {
      param
    }
  }

  private[this] def normalizeBoundaries(boundaries: Boundaries, ctx: LambdaNormContext): Boundaries = boundaries match {
    case Boundaries.Defined(bottom, top) =>
      Boundaries.Defined(normalizeRef(bottom, ctx), normalizeRef(top, ctx))
    case Boundaries.Empty =>
      Boundaries.Empty
  }

  private[this] def normalizeApplied(applied: AppliedReference, ctx: LambdaNormContext): AppliedReference = applied match {
    case NameReference(ref, boundaries, prefix) =>
      NameReference(
        normalizeSym(ref, ctx),
        normalizeBoundaries(boundaries, ctx),
        prefix.map(normalizeApplied(_, ctx))
      )
    case FullReference(symName, parameters, prefix) =>
      FullReference(
        normalizeSym(symName, ctx),
        parameters.map(p => TypeParam(normalizeRef(p.ref, ctx), p.variance)),
        prefix.map(normalizeApplied(_, ctx))
      )
    case IntersectionReference(refs) =>
      maybeIntersection(refs.iterator.map(normalizeApplied(_, ctx)))
    case UnionReference(refs) =>
      maybeUnion(refs.iterator.map(normalizeApplied(_, ctx)))
    case Refinement(reference, decls) =>
      Refinement(normalizeApplied(reference, ctx), decls.map(normalizeDecl(_, ctx)))
    case WildcardReference(boundaries) =>
      WildcardReference(normalizeBoundaries(boundaries, ctx))
  }

  private[this] def normalizeDecl(decl: RefinementDecl, ctx: LambdaNormContext): RefinementDecl = decl match {
    case RefinementDecl.Signature(name, input, output) =>
      RefinementDecl.Signature(name, input.map(normalizeApplied(_, ctx)), normalizeApplied(output, ctx))
    case RefinementDecl.TypeMember(name, ref) =>
      RefinementDecl.TypeMember(name, normalizeRef(ref, ctx))
  }

  private[this] def normalizeRef(ref: AbstractReference, ctx: LambdaNormContext): AbstractReference = ref match {
    case nested: Lambda =>
      normalizeNestedLambda(nested, ctx)
    case applied: AppliedReference =>
      normalizeApplied(applied, ctx)
  }

  def maybeIntersection(r: Set[_ <: LightTypeTagRef]): AppliedReference = maybeIntersection(r.iterator)
  def maybeUnion(r: Set[_ <: LightTypeTagRef]): AppliedReference = maybeUnion(r.iterator)

  sealed trait AppliedNamedReference extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion {
    def asName: NameReference
    def symName: SymName
    def prefix: Option[AppliedReference]
  }

  final case class NameReference(
    ref: SymName,
    boundaries: Boundaries = Boundaries.Empty, // Quirk, we only need it to use NameReferences as Lambda parameters
    prefix: Option[AppliedReference] = None
  ) extends AppliedNamedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

    override def asName: NameReference = this
    override def symName: SymName = ref
  }
  object NameReference {
    @deprecated("Use SymName explicitly", "20.02.2023")
    private[NameReference] def apply(tpeName: String): NameReference = NameReference(SymTypeName(tpeName))
  }

  final case class FullReference(
    symName: SymName,
    parameters: List[TypeParam],
    prefix: Option[AppliedReference] = None
  ) extends AppliedNamedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

    override def asName: NameReference = NameReference(symName, boundaries = Boundaries.Empty, prefix = prefix)

    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def ref: String = SymName.forceName(symName)

    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def this(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]) = {
      this(SymName.SymTypeName(ref), parameters, prefix)
    }

    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def copy(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]): FullReference = {
      this.copy(SymName.SymTypeName(ref), parameters, prefix)
    }
    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def copy$default$1: String = ref

    def copy(symName: SymName = symName, parameters: List[TypeParam] = parameters, prefix: Option[AppliedReference] = prefix): FullReference = {
      new FullReference(symName, parameters, prefix)
    }

  }
  object FullReference extends /*bincompat*/ AbstractFunction3[String, List[TypeParam], Option[AppliedReference], FullReference] {
    @deprecated("bincompat only", "20.02.2023")
    override def apply(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]): FullReference = {
      new FullReference(SymName.SymTypeName(ref), parameters, prefix)
    }
  }

  final case class TypeParam(
    ref: AbstractReference,
    variance: Variance // Quirk, we only need it to simplify/speedup inheritance checks
  ) {
    override def toString: String = this.render()
  }

  sealed trait RefinementDecl {
    def name: String
  }
  object RefinementDecl {
    final case class Signature(name: String, input: List[AppliedReference], output: AppliedReference) extends RefinementDecl
    final case class TypeMember(name: String, ref: AbstractReference) extends RefinementDecl
  }

  sealed trait Variance {
    override final def toString: String = this.render()
  }
  object Variance {
    case object Invariant extends Variance
    case object Contravariant extends Variance
    case object Covariant extends Variance
  }

  sealed trait Boundaries {
    override final def toString: String = this.render()
  }
  object Boundaries {
    final case class Defined(bottom: AbstractReference, top: AbstractReference) extends Boundaries
    case object Empty extends Boundaries
  }

  // this name isn't correct anymore but we keep it here for historical reasons. In fact that should be Symbol or SymRef
  sealed trait SymName {
    // bincompat only
    private[macrortti] def name: String
  }
  object SymName {
    final case class LambdaParamName(index: Int, depth: Int, arity: Int) extends LambdaParameter with SymName {
      @deprecated("bincompat only", "20.02.2023")
      private[macrortti] override def name: String = SymName.forceName(this)
    }

    sealed trait NamedSymbol extends SymName {
      def name: String
    }
    final case class SymTermName(name: String) extends NamedSymbol
    final case class SymTypeName(name: String) extends NamedSymbol
    final case class SymLiteral(name: String) extends NamedSymbol

    object SymLiteral {
      def apply(c: Any): SymLiteral = {
        val constant = c match {
          case s: String => "\"" + s + "\""
          case o => o.toString
        }
        SymLiteral(constant)
      }
    }

    implicit final class SymNameExt(private val name: SymName) extends AnyVal {
      def maybeName: Option[String] = name match {
        case symbol: SymName.NamedSymbol => Some(symbol.name)
        case _: SymName.LambdaParamName => None
      }
    }

    private[macrortti] def forceName(symName: SymName): String = {
      symName match {
        case symbol: SymName.NamedSymbol => symbol.name
        case lpn: LambdaParamName => LTTRenderables.Long.r_LambdaParameterName.render(lpn)
      }
    }

    private[macrortti] def bincompatForceCreateLambdaParamNameFromString(str: String): LambdaParamName = {
      val (numericIndexFromString, numericContextFromString) = {
        val parts = str.split(':')
        val fst = Try(parts(0).toInt)
        val snd = Try(parts(1).toInt)
        (fst, snd) match {
          case (Success(idx), Failure(_)) =>
            (idx, -10)
          case (Success(ctx), Success(idx)) =>
            (idx, ctx)
          case _ =>
            // use MurmurHash as it promises 'high-quality'
            (MurmurHash3.stringHash(str), -10)
        }
      }
      LambdaParamName(numericIndexFromString, numericContextFromString, -10)
    }

  }
}
