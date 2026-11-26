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

/** The structure of a Scala type as a tree of case classes: named types, applied types, type lambdas, intersections, unions,
  * refinements and wildcards. `LightTypeTag#ref` returns it. A `LightTypeTagRef` is either a [[LightTypeTagRef.Lambda]] or an
  * [[LightTypeTagRef.AppliedReference]].
  *
  * {{{
  * val ref = LTT[Map[String, List[Int]]].ref
  *
  * ref.shortName // Map
  * ref.typeArgs  // List(String, List[+Int])
  * ref.toString  // Map[=String,+List[+Int]]
  * }}}
  *
  * @see [[https://zio.dev/izumi-reflect/reference/light-type-tag/ LightTypeTag]]
  */
sealed trait LightTypeTagRef extends LTTSyntax with Serializable {

  // bincompat with versions before 2.3.0
  /** Applies the type lambda to `args`, in order, and returns the result. With fewer arguments than parameters, the result is a lambda over
    * the remaining parameters; with more arguments, the extra ones are ignored. With an empty `args`, returns this reference. Throws a
    * `java.lang.IllegalArgumentException` when `args` is not empty and this reference is not a lambda.
    */
  final def combine(args: Seq[LightTypeTagRef]): AbstractReference = this.combineImpl(args)
  /** Applies the type lambda to the arguments that are `Some`, at the positions they have in `args`; a `None` leaves that parameter open.
    * Throws a `java.lang.IllegalArgumentException` when this reference is not a lambda.
    */
  final def combineNonPos(args: Seq[Option[LightTypeTagRef]]): AbstractReference = this.combineNonPosImpl(args)
  /** Returns the reference without type arguments: lambdas are unwrapped, and a [[LightTypeTagRef.FullReference]] becomes a
    * [[LightTypeTagRef.NameReference]] with the same name and prefix. Members of intersections and unions, and the base of a refinement,
    * are processed the same way.
    */
  final def withoutArgs: AbstractReference = this.withoutArgsImpl
  /** Render to string, omitting package names */
  override final def toString: String = this.toStringImpl
  /** Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  final def repr: String = this.reprImpl
  /** Returns the fully qualified rendering, with unapplied type constructors written in Scala notation, such as
    * `scala.util.Either[+_,+_]`. Lambdas that don't have that simple shape are written as `[A] ➾ scala.collection.immutable.Map[A,+A]`.
    */
  final def scalaStyledRepr: String = this.scalaStyledReprImpl
  /** Deprecated name of `scalaStyledRepr`. */
  @deprecated("Renamed to scalaStyledRepr", "3.0.8")
  final def scalaStyledName: String = this.scalaStyledReprImpl
  /** Returns the name of the type without packages, prefix and type arguments, such as `List` for `List[Int]`. For an intersection or a
    * union, the names of the members are joined with ` & ` or ` | `; for a wildcard, the result is `?`.
    */
  final def shortName: String = this.shortNameImpl
  /** Returns the fully qualified name of the type with its prefix, without type arguments, such as `scala.collection.immutable.List`. */
  final def longNameWithPrefix: String = this.longNameWithPrefixImpl
  /** Returns the fully qualified name of the type's symbol, without type arguments. For an object member `C` in `object Obj`, the result
    * is `Obj.C` on Scala 2 and `Obj\$.C` on Scala 3.
    */
  final def longNameInternalSymbol: String = this.longNameInternalSymbolImpl
  /** Deprecated; returns the same string as `longNameInternalSymbol`. */
  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  final def longName: String = this.longNameInternalSymbolImpl
  /** Returns the prefix of the named type: `Some(Obj)` for `Obj.C[Int]`, and `None` for `List[Int]`. For a lambda or a refinement, it is the
    * prefix of the underlying type; for an intersection or a union, it combines the prefixes of the members; for a wildcard, it is `None`.
    */
  final def getPrefix: Option[AppliedReference] = this.getPrefixImpl
  /** Returns the type arguments of an applied type, such as `List(String, Int)` for `Map[String, Int]`. For a lambda, these are the
    * arguments of its output without the lambda's own parameters; for a refinement, those of its base. For a name, an intersection, a
    * union and a wildcard, the result is empty.
    */
  final def typeArgs: List[AbstractReference] = this.typeArgsImpl
  /** Returns the members of an intersection type. For any other applied reference, returns a set with that reference; for a lambda,
    * returns an empty set.
    */
  final def decompose: Set[LightTypeTagRef.AppliedReferenceExceptIntersection] = this.decomposeImpl
  /** Returns the members of a union type. For any other applied reference, returns a set with that reference; for a lambda, returns an
    * empty set.
    */
  final def decomposeUnion: Set[LightTypeTagRef.AppliedReferenceExceptUnion] = this.decomposeUnionImpl

}

/** The ADT behind `LightTypeTag#ref`. `AbstractReference` is either a `Lambda` or an `AppliedReference`; applied references are names,
  * intersections, unions, refinements and wildcards.
  */
object LightTypeTagRef extends LTTOrdering {
  import LTTRenderables.Short._
//  import LTTRenderables.Long._

  /** A [[LightTypeTagRef.Lambda]] or an [[LightTypeTagRef.AppliedReference]]. */
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

  /** A type lambda with the parameters `input` and the body `output`. An unapplied type constructor such as `List` is the lambda
    * `λ %0 → List[+0]`. Two lambdas are equal when they have the same number of parameters and equal `normalizedOutput`, so parameter names
    * do not matter.
    */
  final case class Lambda(input: List[SymName.LambdaParamName], output: AbstractReference) extends AbstractReference {
    override def hashCode(): Int = {
      normalizedOutput.hashCode()
    }

    /** The number of lambda parameters. */
    lazy val inputSize: Int = input.size
    /** The lambda parameters as [[LightTypeTagRef.NameReference]]s. */
    lazy val paramRefs: Set[NameReference] = input
      .iterator.map {
        n =>
          // No boundary on paramRefs
          // FIXME LambdaParameter should contain bounds and NameReference shouldn't
          //       (Except possibly lower bound of an abstract/opaque type member)
          NameReference(n)
      }.toSet
    /** The names that occur in `output`, as returned by `RuntimeAPI.unpack`; they include the lambda parameters that `output` uses. */
    lazy val referenced: Set[NameReference] = RuntimeAPI.unpack(this)
    /** Returns `true` when `output` uses every lambda parameter. */
    def allArgumentsReferenced: Boolean = paramRefs.diff(referenced).isEmpty
    /** `true` when `output` uses at least one lambda parameter. */
    lazy val someArgumentsReferenced: Boolean = {
      val unusedParamsSize = paramRefs.diff(referenced).size
      unusedParamsSize < paramRefs.size
    }

    /** The lambda parameters renamed to placeholders by position (`0`, `1`, ...). */
    lazy val normalizedParams: List[NameReference] = makeFakeParams.map(_._2)
    /** `output` with the parameters replaced by `normalizedParams`; `equals` and `hashCode` compare it instead of `output`. */
    lazy val normalizedOutput: AbstractReference = RuntimeAPI.applyLambda(this, makeFakeParams)

    override def equals(obj: Any): Boolean = {
      obj match {
        case l: Lambda =>
          inputSize == l.inputSize &&
          (normalizedOutput == l.normalizedOutput)

        case _ =>
          false
      }
    }

    private[this] def makeFakeParams: List[(LambdaParamName, NameReference)] = {
      input.zipWithIndex.map {
        case (p, idx) =>
          p -> NameReference(SymName.LambdaParamName(idx, LightTypeTagRef.LambdaConstants.lambdaFakeParamDepth, inputSize)) // s"!FAKE_$idx"
      }
    }
  }

  /** Depth values of lambda parameters. */
  object LambdaConstants {
    /** Depth (`-1`) that the Scala 2 macros give to lambda parameters that have no enclosing lambda context, as in `LTagK[List]`. */
    final val defaultContextId = -1
    /** Depth of the placeholder parameters in `Lambda#normalizedParams`. */
    final val lambdaFakeParamDepth: Int = -2 // depth is always positive, unless fake
    /** Depth given by `TagMacro` to the lambda parameters it creates. */
    final val tagMacro = -3
  }

  /** A reference that is not a lambda: a name, an applied name, an intersection, a union, a refinement or a wildcard. */
  sealed trait AppliedReference extends AbstractReference

  /** An applied reference that is not an intersection: the type of the members of an [[LightTypeTagRef.IntersectionReference]]. */
  sealed trait AppliedReferenceExceptIntersection extends AppliedReference
  /** An applied reference that is not a union: the type of the members of a [[LightTypeTagRef.UnionReference]]. */
  sealed trait AppliedReferenceExceptUnion extends AppliedReference

  /** The intersection of `refs`, such as `A with B`. Renders as `{A & B}`. */
  final case class IntersectionReference(refs: Set[AppliedReferenceExceptIntersection]) extends AppliedReferenceExceptUnion {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  /** A wildcard type argument with `boundaries`. `List[_]` has `Boundaries.Empty`, and renders as `?`. */
  final case class WildcardReference(boundaries: Boundaries) extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion

  /** The union of `refs`, such as `A | B` on Scala 3. Renders as `{A | B}`. */
  final case class UnionReference(refs: Set[AppliedReferenceExceptUnion]) extends AppliedReferenceExceptIntersection {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  /** A structural type: the type `reference` with the declarations `decls`, such as `CharSequence { def foo(a: Int): Long }`. */
  final case class Refinement(reference: AppliedReference, decls: Set[RefinementDecl]) extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  }

  /** Returns `true` when `t` is a reference to `Any`, `Matchable`, `AnyRef` or `java.lang.Object`; a lambda gives `false`. */
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

  /** Builds the intersection of `refs0`. Nested intersections are flattened. A single distinct member is returned as it is, even when it is
    * `Any`, `Matchable`, `AnyRef` or `java.lang.Object`. Otherwise those four are dropped: a single remaining member is
    * returned as it is, and `Any` is returned when no member remains.
    */
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

  /** Builds the union of `refs0`. Nested unions are flattened and `Nothing` and `Null` are dropped. When a member is `Any`, `Matchable`,
    * `AnyRef` or `java.lang.Object`, the result is that type (the first of them in this order). Otherwise a single remaining member is
    * returned as it is, and `Nothing` is returned when no member remains.
    */
  def maybeUnion(refs0: Iterator[_ <: LightTypeTagRef]): AppliedReference = {
    val refs = refs0.flatMap(_.decomposeUnion).toSet // flatten nested unions
    val normalized = refs -- Set(LightTypeTagInheritance.tpeNothing, LightTypeTagInheritance.tpeNull)
    val superTypes = normalized.intersect(ignored)
    if (superTypes.nonEmpty) {
      if (normalized.contains(LightTypeTagInheritance.tpeAny)) LightTypeTagInheritance.tpeAny
      else if (normalized.contains(LightTypeTagInheritance.tpeMatchable)) LightTypeTagInheritance.tpeMatchable
      else if (normalized.contains(LightTypeTagInheritance.tpeAnyRef)) LightTypeTagInheritance.tpeAnyRef
      else if (normalized.contains(LightTypeTagInheritance.tpeObject)) LightTypeTagInheritance.tpeObject
      else superTypes.head
    } else {
      if (normalized.isEmpty) {
        LightTypeTagInheritance.tpeNothing
      } else if (normalized.size == 1) {
        normalized.head
      } else {
        UnionReference(normalized)
      }
    }
  }

  /** Same as the iterator version, for the members of `r`. */
  def maybeIntersection(r: Set[_ <: LightTypeTagRef]): AppliedReference = maybeIntersection(r.iterator)
  /** Same as the iterator version, for the members of `r`. */
  def maybeUnion(r: Set[_ <: LightTypeTagRef]): AppliedReference = maybeUnion(r.iterator)

  /** A reference to a named type, with or without type arguments. */
  sealed trait AppliedNamedReference extends AppliedReferenceExceptIntersection with AppliedReferenceExceptUnion {
    /** Returns this reference as a [[LightTypeTagRef.NameReference]] with the same name and prefix, and without type arguments. */
    def asName: NameReference
    /** The name of the type. */
    def symName: SymName
    /** The type that contains this type, such as `Obj` for `Obj.C`; `None` when there is none. */
    def prefix: Option[AppliedReference]
  }

  /** A named type without type arguments, such as `Int` or `Obj::C`. `boundaries` hold the bounds of an abstract type (`HasT::T|<Int..AnyVal>`),
    * and `prefix` the type that contains it.
    */
  final case class NameReference(
    ref: SymName,
    boundaries: Boundaries = Boundaries.Empty, // Quirk, we only need it to use NameReferences as Lambda parameters
    prefix: Option[AppliedReference] = None
  ) extends AppliedNamedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

    override def asName: NameReference = this
    override def symName: SymName = ref
  }
  /** Companion of [[LightTypeTagRef.NameReference]]. */
  object NameReference {
    @deprecated("Use SymName explicitly", "20.02.2023")
    private[NameReference] def apply(tpeName: String): NameReference = NameReference(SymTypeName(tpeName))
  }

  /** A named type applied to type arguments, such as `List[+Int]`. `parameters` are the arguments with their variance. */
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

    /** Returns a copy with the given fields replaced. */
    def copy(symName: SymName = symName, parameters: List[TypeParam] = parameters, prefix: Option[AppliedReference] = prefix): FullReference = {
      new FullReference(symName, parameters, prefix)
    }

  }
  /** Companion of [[LightTypeTagRef.FullReference]]. */
  object FullReference extends /*bincompat*/ AbstractFunction3[String, List[TypeParam], Option[AppliedReference], FullReference] {
    @deprecated("bincompat only", "20.02.2023")
    override def apply(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]): FullReference = {
      new FullReference(SymName.SymTypeName(ref), parameters, prefix)
    }
  }

  /** A type argument `ref` with the declared `variance` of the type parameter it fills (`List[+Int]` holds `+Int`). */
  final case class TypeParam(
    ref: AbstractReference,
    variance: Variance // Quirk, we only need it to simplify/speedup inheritance checks
  ) {
    override def toString: String = this.render()
  }

  /** A member of a [[LightTypeTagRef.Refinement]]. */
  sealed trait RefinementDecl {
    /** The name of the member. */
    def name: String
  }
  /** Companion of [[LightTypeTagRef.RefinementDecl]]. */
  object RefinementDecl {
    /** A method declaration `def name(input): output`. */
    final case class Signature(name: String, input: List[AppliedReference], output: AppliedReference) extends RefinementDecl
    /** A type member declaration. For an alias `type T = Int`, `ref` is `Int`; for an abstract member such as `type T <: Int`, `ref` is a
      * [[LightTypeTagRef.NameReference]] named after the member, with its bounds (`T|<Nothing..Int>`).
      */
    final case class TypeMember(name: String, ref: AbstractReference) extends RefinementDecl
  }

  /** The variance of a type parameter. Renders as `=`, `-` or `+`. */
  sealed trait Variance {
    override final def toString: String = this.render()
  }
  /** Companion of [[LightTypeTagRef.Variance]]. */
  object Variance {
    /** Invariant; renders as `=`. */
    case object Invariant extends Variance
    /** Contravariant; renders as `-`. */
    case object Contravariant extends Variance
    /** Covariant; renders as `+`. */
    case object Covariant extends Variance
  }

  /** The bounds of a type: [[LightTypeTagRef.Boundaries.Defined]] or [[LightTypeTagRef.Boundaries.Empty]]. */
  sealed trait Boundaries {
    override final def toString: String = this.render()
  }
  /** Companion of [[LightTypeTagRef.Boundaries]]. */
  object Boundaries {
    /** A lower bound `bottom` and an upper bound `top`. Renders as `<bottom..top>`. */
    final case class Defined(bottom: AbstractReference, top: AbstractReference) extends Boundaries
    /** No bounds; renders as an empty string. */
    case object Empty extends Boundaries
  }

  // this name isn't correct anymore but we keep it here for historical reasons. In fact that should be Symbol or SymRef
  /** The name of a symbol: a term, a type, a literal or a lambda parameter. */
  sealed trait SymName {
    // bincompat only
    private[macrortti] def name: String
  }
  /** Companion of [[LightTypeTagRef.SymName]]. */
  object SymName {
    /** The parameter at position `index` of a lambda with `arity` parameters. `depth` is printed in front of the index (`3:1`) when it is
      * positive.
      */
    final case class LambdaParamName(index: Int, depth: Int, arity: Int) extends LambdaParameter with SymName {
      @deprecated("bincompat only", "20.02.2023")
      private[macrortti] override def name: String = SymName.forceName(this)
    }

    /** A symbol with a name. */
    sealed trait NamedSymbol extends SymName {
      /** The name of the symbol. */
      def name: String
    }
    /** The name of a term, such as an object (`Obj` for `Obj.type`). */
    final case class SymTermName(name: String) extends NamedSymbol
    /** The name of a type, such as `scala.Int`. */
    final case class SymTypeName(name: String) extends NamedSymbol
    /** The name of a literal type; string literals include their quotes (`"abc"`). */
    final case class SymLiteral(name: String) extends NamedSymbol

    /** Companion of [[LightTypeTagRef.SymName.SymLiteral]]. */
    object SymLiteral {
      /** Creates a literal name from `c`: a `String` gets quotes, other values use `toString`. A call with an argument whose static type is
        * `String` uses the case class `apply` and adds no quotes.
        */
      def apply(c: Any): SymLiteral = {
        val constant = c match {
          case s: String => "\"" + s + "\""
          case o => o.toString
        }
        SymLiteral(constant)
      }
    }

    /** Adds `maybeName` to a [[LightTypeTagRef.SymName]]. */
    implicit final class SymNameExt(private val name: SymName) extends AnyVal {
      /** Returns the name for a term, type or literal, and `None` for a lambda parameter. */
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
