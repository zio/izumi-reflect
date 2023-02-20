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
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.annotation.tailrec
import scala.util.Sorting

sealed trait LightTypeTagRef extends Serializable {

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
  override final def toString: String = {
    import izumi.reflect.macrortti.LTTRenderables.Short._
    (this: LightTypeTagRef).render()
  }

  /** Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  final def repr: String = {
    import izumi.reflect.macrortti.LTTRenderables.Long._
    (this: LightTypeTagRef).render()
  }

  final def shortName: String = {
    getName(r => LTTRenderables.Short.r_SymName(r.symName, hasPrefix = false))
  }

  final def longNameWithPrefix: String = {
    getName(r => LTTRenderables.LongPrefixDot.r_NameRefRenderer.render(NameReference(r.symName, Boundaries.Empty, r.prefix)))
  }

  final def longNameInternalSymbol: String = {
    getName(r => LTTRenderables.Long.r_SymName(r.symName, hasPrefix = false))
  }

  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  final def longName: String = {
    longNameInternalSymbol
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
      case WildcardReference(_) =>
        Nil
      case Refinement(reference, _) =>
        reference.typeArgs
    }
  }

  /** decompose intersection type */
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

  private[macrortti] final def applySeq(refs: Seq[AbstractReference]): AbstractReference = {
    applyParameters {
      l =>
        l.input.zip(refs).map {
          case (p, v) =>
            p.name -> v
        }
    }
  }

  private[macrortti] final def applyParameters(p: Lambda => Seq[(String, AbstractReference)]): AbstractReference = {
    this match {
      case l: Lambda =>
        val parameters = p(l)
        if (l.input.size < parameters.size) {
          throw new IllegalArgumentException(s"$this expects no more than ${l.input.size} parameters: ${l.input} but got $parameters")
        }
        val expected = l.input.iterator.map(_.name).toSet
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

object LightTypeTagRef {
  import LTTRenderables.Short._
//  import LTTRenderables.Long._

  sealed trait AbstractReference extends LightTypeTagRef

  final case class Lambda(input: List[LambdaParameter], output: AbstractReference) extends AbstractReference {
    override def hashCode(): Int = {
      normalizedOutput.hashCode()
    }

    lazy val paramRefs: Set[NameReference] = input
      .iterator.map {
        n =>
          // No boundary on paramRefs
          // FIXME LambdaParameter should contain bounds and NameReference shouldn't
          //       (Except possibly lower bound of an abstract/opaque type member)
          NameReference(SymName.LambdaParamName(n.name))
      }.toSet
    lazy val referenced: Set[NameReference] = RuntimeAPI.unpack(this)
    def allArgumentsReferenced: Boolean = paramRefs.diff(referenced).isEmpty
    lazy val someArgumentsReferenced: Boolean = paramRefs.diff(referenced).size < referenced.size

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

    private[this] def makeFakeParams: List[(String, NameReference)] = {
      input.zipWithIndex.map {
        case (p, idx) =>
          p.name -> NameReference(SymName.LambdaParamName(s"!FAKE_$idx"))
      }
    }
  }

  final case class LambdaParameter(name: String) {
    override def toString: String = this.render()
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
    ref: String,
    parameters: List[TypeParam],
    prefix: Option[AppliedReference] = None
  ) extends AppliedNamedReference {
    override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

    override def asName: NameReference = NameReference(symName, prefix = prefix)
    override def symName: SymName = SymTypeName(ref)
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
    final case class LambdaParamName(name: String) extends SymName
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
        val compare1 = Ordering.String.compare(refx, refy)
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
        case LambdaParamName(_) => 3
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
