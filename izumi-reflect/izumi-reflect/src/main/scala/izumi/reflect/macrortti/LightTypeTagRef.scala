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

import izumi.reflect.internal.OrderingCompat
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{LambdaParamName, SymLiteral, SymTermName, SymTypeName}

import scala.util.Sorting



sealed trait LightTypeTagRef extends Serializable with LTTSyntax {}

object LightTypeTagRef {
  import LTTRenderables.Short._
//  import LTTRenderables.Long._

  sealed trait AbstractReference extends LightTypeTagRef

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

    private[LightTypeTagRef] def compare(y: Lambda): Int = {
      val x = this
      // Mirror equals
      val compare1 = Ordering.Int.compare(x.input.size, y.input.size)
      if (compare1 != 0) return compare1
      OrderingAbstractReference.compare(x.normalizedOutput, y.normalizedOutput)
    }

    private[this] def makeFakeParams: List[(LambdaParamName, NameReference)] = {
      input.zipWithIndex.map {
        case (p, idx) =>
          p -> NameReference(SymName.LambdaParamName(idx, -2, input.size)) // s"!FAKE_$idx"
      }
    }
  }

//  final case class LambdaParameter(name: SymName.LambdaParamName) {
//    override def toString: String = this.render()
//  }

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
    private[LightTypeTagRef] def ref: String = symName.name

    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def this(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]) = {
      this(SymName.SymTypeName(ref), parameters, prefix)
    }
  }
  object FullReference {
    @deprecated("bincompat only", "20.02.2023")
    private[LightTypeTagRef] def apply(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference]): FullReference = {
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

  sealed trait SymName {
    def name: String
  }
  object SymName {
    final case class SymTermName(name: String) extends SymName
    final case class LambdaParamName(index: Int, depth: Int, arity: Int) extends SymName {
      override def name: String = s"$depth:$index/$arity"
    }
    final case class SymTypeName(name: String) extends SymName
    final case class SymLiteral(name: String) extends SymName
    object SymLiteral {
      def apply(c: Any): SymLiteral = {
        val constant = c match {
          case s: String => "\"" + s + "\""
          case o => o.toString
        }
        SymLiteral(constant)
      }
    }
  }

  @inline private[macrortti] final def OrderingAbstractReferenceInstance[A <: AbstractReference]: Ordering[A] = OrderingAbstractReference.asInstanceOf[Ordering[A]]
  @inline private[macrortti] final def OrderingRefinementDeclInstance: Ordering[RefinementDecl] = OrderingRefinementDecl

  private[this] val OrderingAbstractReference: Ordering[AbstractReference] = new Ordering[AbstractReference] {
    override def equiv(x: AbstractReference, y: AbstractReference): Boolean = x == y

    override def compare(x: AbstractReference, y: AbstractReference): Int = (x, y) match {
      case (lx: Lambda, ly: Lambda) =>
        // Mirror Lambda#equals
        lx.compare(ly)

      case (IntersectionReference(refsx), IntersectionReference(refsy)) =>
        OrderingArrayAbstractReference.compare(refSetToSortedArray(refsx), refSetToSortedArray(refsy))

      case (UnionReference(refsx), UnionReference(refsy)) =>
        OrderingArrayAbstractReference.compare(refSetToSortedArray(refsx), refSetToSortedArray(refsy))

      case (Refinement(referencex, declsx), Refinement(referencey, declsy)) =>
        val compare1 = compare(referencex, referencey)
        if (compare1 != 0) return compare1
        OrderingArrayRefinementDecl.compare(refinementDeclSetToSortedArray(declsx), refinementDeclSetToSortedArray(declsy))

      case (NameReference(symx, boundariesx, prefixx), NameReference(symy, boundariesy, prefixy)) =>
        val compare1 = OrderingSymName.compare(symx, symy)
        if (compare1 != 0) return compare1
        val compare2 = OrderingBoundaries.compare(boundariesx, boundariesy)
        if (compare2 != 0) return compare2
        OrderingOptionAbstractReference.compare(prefixx, prefixy)

      case (FullReference(refx, parametersx, prefixx), FullReference(refy, parametersy, prefixy)) =>
        val compare1 = OrderingSymName.compare(refx, refy)
        if (compare1 != 0) return compare1
        val compare2 = OrderingListTypeParam.compare(parametersx, parametersy)
        if (compare2 != 0) return compare2
        OrderingOptionAbstractReference.compare(prefixx, prefixy)

      case _ =>
        def idx(abstractReference: AbstractReference): Int = abstractReference match {
          case _: Lambda => 0
          case _: IntersectionReference => 1
          case _: UnionReference => 2
          case _: Refinement => 3
          case _: NameReference => 4
          case _: FullReference => 5
          case _: WildcardReference => 6
        }
        Ordering.Int.compare(idx(x), idx(y))
    }
  }

  private[macrortti] def refSetToSortedArray[T <: AbstractReference](set: Set[_ <: T]): Array[T] = {
    @inline implicit def OrderingInstance: Ordering[AbstractReference] = LightTypeTagRef.OrderingAbstractReferenceInstance
    val array: Array[AbstractReference] = set.toArray
    Sorting.stableSort(array)
    array.asInstanceOf[Array[T]]
  }

  private[macrortti] def refinementDeclSetToSortedArray(set: Set[RefinementDecl]): Array[RefinementDecl] = {
    @inline implicit def OrderingInstance: Ordering[RefinementDecl] = LightTypeTagRef.OrderingRefinementDeclInstance
    val array: Array[RefinementDecl] = set.toArray
    Sorting.stableSort(array)
    array
  }

  private[this] val OrderingRefinementDecl: Ordering[RefinementDecl] = new Ordering[RefinementDecl] {
    override def equiv(x: RefinementDecl, y: RefinementDecl): Boolean = x == y

    override def compare(x: RefinementDecl, y: RefinementDecl): Int = (x, y) match {
      case (RefinementDecl.Signature(namex, inputx, outputx), RefinementDecl.Signature(namey, inputy, outputy)) =>
        val compare1 = Ordering.String.compare(namex, namey)
        if (compare1 != 0) return compare1
        val compare2 = OrderingListAbstractReference.compare(inputx, inputy)
        if (compare2 != 0) return compare2
        OrderingAbstractReference.compare(outputx, outputy)

      case (RefinementDecl.TypeMember(namex, refx), RefinementDecl.TypeMember(namey, refy)) =>
        val compare1 = Ordering.String.compare(namex, namey)
        if (compare1 != 0) return compare1
        OrderingAbstractReference.compare(refx, refy)

      case _ =>
        def idx(refinementDecl: RefinementDecl): Int = refinementDecl match {
          case _: RefinementDecl.Signature => 0
          case _: RefinementDecl.TypeMember => 1
        }
        Ordering.Int.compare(idx(x), idx(y))
    }
  }

  private[this] val OrderingSymName: Ordering[SymName] = new Ordering[SymName] {
    override def equiv(x: SymName, y: SymName): Boolean = x == y

    override def compare(x: SymName, y: SymName): Int = {
      def idx(symName: SymName): Int = symName match {
        case SymTermName(_) => 0
        case SymTypeName(_) => 1
        case SymLiteral(_) => 2
        case LambdaParamName(_, _, _) => 3
      }
      val compare1 = Ordering.Int.compare(idx(x), idx(y))
      if (compare1 != 0) return compare1
      Ordering.String.compare(x.name, y.name)
    }
  }

  private[this] val OrderingBoundaries: Ordering[Boundaries] = new Ordering[Boundaries] {
    override def equiv(x: Boundaries, y: Boundaries): Boolean = x == y

    override def compare(x: Boundaries, y: Boundaries): Int = (x, y) match {
      case (Boundaries.Defined(rebx, retx), Boundaries.Defined(reby, rety)) =>
        val compare1 = OrderingAbstractReference.compare(rebx, reby)
        if (compare1 != 0) return compare1
        OrderingAbstractReference.compare(retx, rety)

      case (x, y) =>
        def idx(boundaries: Boundaries): Int = boundaries match {
          case _: Boundaries.Empty.type => 0
          case _: Boundaries.Defined => 1
        }
        Ordering.Int.compare(idx(x), idx(y))
    }
  }

  private[this] val OrderingTypeParam: Ordering[TypeParam] = new Ordering[TypeParam] {
    override def equiv(x: TypeParam, y: TypeParam): Boolean = x == y

    override def compare(x: TypeParam, y: TypeParam): Int = (x, y) match {
      case (TypeParam(namex, varx), TypeParam(namey, vary)) =>
        val compare1 = OrderingAbstractReference.compare(namex, namey)
        if (compare1 != 0) return compare1
        OrderingVariance.compare(varx, vary)
    }
  }

  private[this] val OrderingVariance: Ordering[Variance] = Ordering.by {
    case Variance.Invariant => 0
    case Variance.Contravariant => 1
    case Variance.Covariant => 2
  }

  private[this] val OrderingListAbstractReference: Ordering[List[AbstractReference]] = OrderingCompat.listOrdering(OrderingAbstractReference)
  private[this] val OrderingArrayAbstractReference: Ordering[Array[AbstractReference]] = OrderingCompat.arrayOrdering(OrderingAbstractReference)
  private[this] val OrderingOptionAbstractReference: Ordering[Option[AbstractReference]] = Ordering.Option(OrderingAbstractReference)

  private[this] val OrderingArrayRefinementDecl: Ordering[Array[RefinementDecl]] = OrderingCompat.arrayOrdering(OrderingRefinementDecl)

  private[this] val OrderingListTypeParam: Ordering[List[TypeParam]] = OrderingCompat.listOrdering(OrderingTypeParam)
}
