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

import izumi.reflect.macrortti.LightTypeTagRef.AbstractReference
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
  final def scalaStyledName: String = this.scalaStyledNameImpl
  final def shortName: String = this.shortNameImpl
  final def longNameWithPrefix: String = this.longNameWithPrefixImpl
  final def longNameInternalSymbol: String = this.longNameInternalSymbolImpl
  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  final def longName: String = this.longNameInternalSymbolImpl
  final def getPrefix: Option[LightTypeTagRef] = this.getPrefixImpl
  final def typeArgs: List[AbstractReference] = this.typeArgsImpl
  /** decompose intersection type */
  final def decompose: Set[LightTypeTagRef.AppliedReference] = this.decomposeImpl
  final def decomposeUnion: Set[LightTypeTagRef.AppliedReference] = this.decomposeUnionImpl

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

    lazy val normalizedParams: List[NameReference] = makeFakeParams.map(_._2)
    lazy val normalizedOutput: AbstractReference = RuntimeAPI.applyLambda(this, makeFakeParams)

    override def equals(obj: Any): Boolean = {
      obj match {
        case l: Lambda =>
          input.size == l.input.size &&
          (normalizedOutput == l.normalizedOutput)

        case _ =>
          false
      }
    }

    private[this] def makeFakeParams: List[(LambdaParamName, NameReference)] = {
      input.zipWithIndex.map {
        case (p, idx) =>
          p -> NameReference(SymName.LambdaParamName(idx, -2, input.size)) // s"!FAKE_$idx"
      }
    }
  }

  sealed trait AppliedReference extends AbstractReference

  final case class IntersectionReference(refs: Set[AppliedReference]) extends AppliedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  final case class WildcardReference(boundaries: Boundaries) extends AppliedReference

  final case class UnionReference(refs: Set[AppliedReference]) extends AppliedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  final case class Refinement(reference: AppliedReference, decls: Set[RefinementDecl]) extends AppliedReference {
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

  private[reflect] val ignored = Set[AppliedReference](
    LightTypeTagInheritance.tpeAny,
    LightTypeTagInheritance.tpeAnyRef,
    LightTypeTagInheritance.tpeObject,
    LightTypeTagInheritance.tpeMatchable
  )

  def maybeIntersection(refs: Set[AppliedReference]): AppliedReference = {
    val normalized = refs.diff(ignored)
    normalized.toList match {
      case Nil =>
        LightTypeTagInheritance.tpeAny
      case head :: Nil =>
        head
      case _ =>
        IntersectionReference(normalized)
    }
  }

  def maybeUnion(refs: Set[AppliedReference]): AppliedReference = {
    val normalized = refs.diff(ignored)
    normalized.toList match {
      case Nil =>
        LightTypeTagInheritance.tpeAny
      case head :: Nil =>
        head
      case _ =>
        UnionReference(normalized)
    }
  }

  sealed trait AppliedNamedReference extends AppliedReference {
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

    override def asName: NameReference = NameReference(symName, prefix = prefix)

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
