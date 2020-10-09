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
import izumi.reflect.internal.OrderingCompat.setToSortedSet
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymLiteral, SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

sealed trait LightTypeTagRef {
  final def combine(args: Seq[LightTypeTagRef]): AbstractReference = {
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

  final def combineNonPos(args: Seq[Option[LightTypeTagRef]]): AbstractReference = {
    applyParameters {
      l =>
        l.input.zip(args).flatMap {
          case (p, v) =>
            v match {
              case Some(value: AbstractReference) =>
                Seq(p.name -> value)
              case None =>
                Seq.empty
            }
        }
    }
  }

  final def withoutArgs: AbstractReference = {
    def appliedNamedReference(reference: AppliedNamedReference) = {
      reference match {
        case LightTypeTagRef.NameReference(_, _, _) => reference
        case LightTypeTagRef.FullReference(ref, parameters @ _, prefix) => NameReference(SymTypeName(ref), Boundaries.Empty, prefix)
      }
    }

    def appliedReference(reference: AppliedReference): AppliedReference = {
      reference match {
        case reference: AppliedNamedReference => appliedNamedReference(reference)
        case LightTypeTagRef.IntersectionReference(refs) =>
          LightTypeTagRef.maybeIntersection(refs.map(appliedReference))
        case LightTypeTagRef.UnionReference(refs) =>
          LightTypeTagRef.maybeUnion(refs.map(appliedReference))
        case LightTypeTagRef.Refinement(reference, decls) =>
          LightTypeTagRef.Refinement(appliedReference(reference), decls)
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

  final def shortName: String = {
    getName(LTTRenderables.Short.r_SymName(_, hasPrefix = false), this)
  }

  final def longName: String = {
    getName(LTTRenderables.Long.r_SymName(_, hasPrefix = false), this)
  }

  @tailrec
  @inline
  private[this] final def getName(render: SymName => String, self: LightTypeTagRef): String = {
    self match {
      case Lambda(_, output) => getName(render, output)
      case NameReference(ref, _, _) => render(ref)
      case FullReference(ref, _, _) => render(SymTypeName(ref))
      case IntersectionReference(refs) => refs.map(_.shortName).mkString(" & ")
      case UnionReference(refs) => refs.map(_.shortName).mkString(" | ")
      case Refinement(reference, _) => getName(render, reference)
    }
  }

  final def getPrefix: Option[LightTypeTagRef] = {
    @tailrec
    @inline
    def getPrefix(self: LightTypeTagRef): Option[LightTypeTagRef] = {
      self match {
        case Lambda(_, output) => getPrefix(output)
        case NameReference(_, _, prefix) => prefix
        case FullReference(_, _, prefix) => prefix
        case IntersectionReference(refs) =>
          val prefixes = refs.map(_.getPrefix).collect {
            case Some(p: AppliedReference) => p
          }
          if (prefixes.nonEmpty) Some(maybeIntersection(prefixes)) else None
        case UnionReference(refs) =>
          val prefixes = refs.map(_.getPrefix).collect {
            case Some(p: AppliedReference) => p
          }
          if (prefixes.nonEmpty) Some(maybeUnion(prefixes)) else None
        case Refinement(reference, _) => getPrefix(reference)
      }
    }

    getPrefix(this)
  }

  final def typeArgs: List[AbstractReference] = {
    this match {
      case Lambda(input, output) =>
        val params = input.iterator.map(_.name).toSet
        output.typeArgs.filter {
          case n: AppliedNamedReference =>
            !params.contains(n.asName.ref.name)
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
      case Refinement(reference, _) =>
        reference.typeArgs
    }
  }

  final def decompose: Set[AppliedReference] = {
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

  final def decomposeUnion: Set[AppliedReference] = {
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

  private[macrortti] def applySeq(refs: Seq[AbstractReference]): AbstractReference = {
    applyParameters {
      l =>
        l.input.zip(refs).map {
          case (p, v) =>
            p.name -> v
        }
    }
  }
  private[macrortti] def applyParameters(p: Lambda => Seq[(String, AbstractReference)]): AbstractReference = {
    this match {
      case l: Lambda =>
        val parameters = p(l)
        if (l.input.size < parameters.size) {
          throw new IllegalArgumentException(s"$this expects no more than ${l.input.size} parameters: ${l.input} but got $parameters")
        }
        val expected = l.input.map(_.name).toSet
        val unknownKeys = parameters.map(_._1).toSet.diff(expected)
        if (unknownKeys.nonEmpty) {
          throw new IllegalArgumentException(s"$this takes parameters: $expected but got unexpected ones: $unknownKeys")
        }

        RuntimeAPI.applyLambda(l, parameters)
      case _ =>
        throw new IllegalArgumentException(s"$this is not a type lambda, it cannot be parameterized")
    }
  }
}

object LightTypeTagRef {
  import LTTRenderables.Short._
//  import LTTRenderables.Long._

  sealed trait AbstractReference extends LightTypeTagRef

  final case class Lambda(input: List[LambdaParameter], output: AbstractReference) extends AbstractReference {
    override def hashCode(): Int = {
      normalizedOutput.hashCode()
    }

    def referenced: Set[NameReference] = RuntimeAPI.unpack(this)
    def paramRefs: Set[NameReference] = input.map(n => NameReference(n.name)).toSet
    def allArgumentsReferenced: Boolean = paramRefs.diff(referenced).isEmpty

    lazy val normalizedParams: List[NameReference] = makeFakeParams.map(_._2)
    lazy val normalizedOutput: AbstractReference = RuntimeAPI.applyLambda(this, makeFakeParams)

    override def equals(obj: Any): Boolean = {
      obj match {
        case l: Lambda =>
          if (input.size == l.input.size) {
            normalizedOutput == l.normalizedOutput
          } else {
            false
          }

        case _ =>
          false
      }
    }

    override def toString: String = this.render()

    private[this] def makeFakeParams: List[(String, NameReference)] = {
      input.zipWithIndex.map {
        case (p, idx) =>
          p.name -> NameReference(s"!FAKE_$idx")
      }
    }
  }

  final case class LambdaParameter(name: String) {
    override def toString: String = this.render()
  }

  sealed trait AppliedReference extends AbstractReference

  final case class IntersectionReference(refs: Set[AppliedReference]) extends AppliedReference {
    override lazy val hashCode: Int = super.hashCode()
    override def toString: String = this.render()
  }

  final case class UnionReference(refs: Set[AppliedReference]) extends AppliedReference {
    override lazy val hashCode: Int = super.hashCode()
    override def toString: String = this.render()
  }

  final case class Refinement(reference: AppliedReference, decls: Set[RefinementDecl]) extends AppliedReference {
    override lazy val hashCode: Int = super.hashCode()
    override def toString: String = this.render()
  }

  private[this] val eradicate = Set[AppliedReference](
    LightTypeTagInheritance.tpeAny,
    LightTypeTagInheritance.tpeAnyRef,
    LightTypeTagInheritance.tpeObject
  )

  def maybeIntersection(refs: Set[AppliedReference]): AppliedReference = {
    val normalized = refs.diff(eradicate)
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
    val normalized = refs.diff(eradicate)
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
  }

  final case class NameReference(ref: SymName, boundaries: Boundaries = Boundaries.Empty, prefix: Option[AppliedReference] = None) extends AppliedNamedReference {
    override lazy val hashCode: Int = super.hashCode()

    override def asName: NameReference = this

    override def toString: String = this.render()
  }
  object NameReference {
    def apply(tpeName: String): NameReference = NameReference(SymTypeName(tpeName))
  }

  final case class FullReference(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference] = None) extends AppliedNamedReference {
    override lazy val hashCode: Int = super.hashCode()

    override def asName: NameReference = NameReference(SymTypeName(ref), prefix = prefix)

    override def toString: String = this.render()
  }

  final case class TypeParam(ref: AbstractReference, variance: Variance) {
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

  private[reflect] implicit def OrderingAbstractReferenceInstance[A <: AbstractReference]: Ordering[A] = OrderingAbstractReference.asInstanceOf[Ordering[A]]

  private[this] val OrderingVariance: Ordering[Variance] = Ordering.by {
    case Variance.Invariant => 0
    case Variance.Contravariant => 1
    case Variance.Covariant => 2
  }
  private[this] val OrderingLambdaParameter: Ordering[LambdaParameter] = Ordering.by((_: LambdaParameter).name)
  private[this] val OrderingListLambdaParameter: Ordering[List[LambdaParameter]] = OrderingCompat.listOrdering(OrderingLambdaParameter)
  private[this] val OrderingSymName: Ordering[SymName] = Ordering.fromLessThan {
    case (SymTermName(namex), SymTermName(namey)) => Ordering.String.lt(namex, namey)
    case (SymTypeName(namex), SymTypeName(namey)) => Ordering.String.lt(namex, namey)
    case (SymLiteral(namex), SymLiteral(namey)) => Ordering.String.lt(namex, namey)
    case (x, y) =>
      def idx(symName: SymName): Int = symName match {
        case SymTermName(_) => 0
        case SymTypeName(_) => 1
        case SymLiteral(_) => 2
      }
      idx(x) < idx(y)
  }
  private[this] val OrderingAbstractReference: Ordering[AbstractReference] = Ordering.fromLessThan {
    case (Lambda(inputx, outputx), Lambda(inputy, outputy)) =>
      OrderingAbstractReference.lt(outputx, outputy) ||
      OrderingListLambdaParameter.lt(inputx, inputy)
    case (IntersectionReference(refsx), IntersectionReference(refsy)) =>
      OrderingSortedSetAbstractReference.lt(
        setToSortedSet[AbstractReference](OrderingAbstractReference)(refsx),
        setToSortedSet[AbstractReference](OrderingAbstractReference)(refsy)
      )
    case (UnionReference(refsx), UnionReference(refsy)) =>
      OrderingSortedSetAbstractReference.lt(
        setToSortedSet[AbstractReference](OrderingAbstractReference)(refsx),
        setToSortedSet[AbstractReference](OrderingAbstractReference)(refsy)
      )
    case (Refinement(referencex, declsx), Refinement(referencey, declsy)) =>
      OrderingAbstractReference.lt(referencex, referencey) ||
      OrderingSortedSetRefinementDecl.lt(
        setToSortedSet(OrderingRefinementDecl)(declsx),
        setToSortedSet(OrderingRefinementDecl)(declsy)
      )
    case (NameReference(refx, boundariesx, prefixx), NameReference(refy, boundariesy, prefixy)) =>
      OrderingSymName.lt(refx, refy) ||
      OrderingBoundaries.lt(boundariesx, boundariesy) ||
      OrderingOptionAbstractReference.lt(prefixx, prefixy)
    case (FullReference(refx, parametersx, prefixx), FullReference(refy, parametersy, prefixy)) =>
      Ordering.String.lt(refx, refy) ||
      OrderingListTypeParam.lt(parametersx, parametersy) ||
      OrderingOptionAbstractReference.lt(prefixx, prefixy)
    case (x, y) =>
      def idx(abstractReference: AbstractReference): Int = abstractReference match {
        case _: Lambda => 0
        case _: IntersectionReference => 1
        case _: UnionReference => 2
        case _: Refinement => 3
        case _: NameReference => 4
        case _: FullReference => 5
      }
      idx(x) < idx(y)
  }
  private[this] val OrderingListAbstractReference: Ordering[List[AbstractReference]] = OrderingCompat.listOrdering(OrderingAbstractReference)
  private[this] val OrderingSortedSetAbstractReference: Ordering[SortedSet[AbstractReference]] = OrderingCompat.sortedSetOrdering(OrderingAbstractReference)
  private[this] val OrderingOptionAbstractReference: Ordering[Option[AbstractReference]] = Ordering.Option(OrderingAbstractReference)
  private[reflect] implicit val OrderingRefinementDecl: Ordering[RefinementDecl] = Ordering.fromLessThan {
    case (RefinementDecl.Signature(namex, inputx, outputx), RefinementDecl.Signature(namey, inputy, outputy)) =>
      Ordering.String.lt(namex, namey) ||
      OrderingListAbstractReference.lt(inputx, inputy) ||
      OrderingAbstractReference.lt(outputx, outputy)
    case (RefinementDecl.TypeMember(namex, refx), RefinementDecl.TypeMember(namey, refy)) =>
      Ordering.String.lt(namex, namey) ||
      OrderingAbstractReference.lt(refx, refy)
    case (x, y) =>
      def idx(refinementDecl: RefinementDecl) = refinementDecl match {
        case _: RefinementDecl.Signature => 0
        case _: RefinementDecl.TypeMember => 1
      }
      idx(x) < idx(y)
  }
  private[this] val OrderingSortedSetRefinementDecl: Ordering[SortedSet[RefinementDecl]] = OrderingCompat.sortedSetOrdering(OrderingRefinementDecl)
  private[this] val OrderingBoundaries: Ordering[Boundaries] = Ordering.fromLessThan {
    case (Boundaries.Defined(rebx, retx), Boundaries.Defined(reby, rety)) =>
      OrderingAbstractReference.lt(rebx, reby) ||
      OrderingAbstractReference.lt(retx, rety)
    case (x, y) =>
      def idx(boundaries: Boundaries) = boundaries match {
        case _: Boundaries.Empty.type => 0
        case _: Boundaries.Defined => 1
      }
      idx(x) < idx(y)
  }
  private[this] val OrderingTypeParam: Ordering[TypeParam] = Ordering.fromLessThan {
    case (TypeParam(namex, varx), TypeParam(namey, vary)) =>
      OrderingAbstractReference.lt(namex, namey) ||
      OrderingVariance.lt(varx, vary)
  }
  private[this] val OrderingListTypeParam: Ordering[List[TypeParam]] = OrderingCompat.listOrdering(OrderingTypeParam)
}
