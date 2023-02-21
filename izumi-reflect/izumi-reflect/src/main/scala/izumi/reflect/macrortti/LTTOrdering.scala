package izumi.reflect.macrortti

import izumi.reflect.internal.OrderingCompat
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{LambdaParamName, SymLiteral, SymTermName, SymTypeName}

import scala.util.Sorting

trait LTTOrdering {
  import LightTypeTagRef._

  @inline private[macrortti] final def OrderingAbstractReferenceInstance[A <: AbstractReference]: Ordering[A] = OrderingAbstractReference.asInstanceOf[Ordering[A]]

  @inline private[macrortti] final def OrderingRefinementDeclInstance: Ordering[RefinementDecl] = OrderingRefinementDecl

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

  private[this] val OrderingAbstractReference: Ordering[AbstractReference] = new Ordering[AbstractReference] {
    override def equiv(x: AbstractReference, y: AbstractReference): Boolean = x == y

    override def compare(x: AbstractReference, y: AbstractReference): Int = (x, y) match {
      case (lx: Lambda, ly: Lambda) =>
        // Mirror Lambda#equals
        val compare1 = Ordering.Int.compare(lx.input.size, ly.input.size)
        if (compare1 != 0) return compare1
        OrderingAbstractReference.compare(lx.normalizedOutput, ly.normalizedOutput)

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

      def asStr(v: SymName.LambdaParamName) = LTTRenderables.Long.r_LambdaParameterName.render(v)

      (x, y) match {
        case (x1: SymName.NamedSymbol, y1: SymName.NamedSymbol) => Ordering.String.compare(x1.name, y1.name)
        case (x1: SymName.LambdaParamName, y1: SymName.LambdaParamName) =>
          Ordering.Tuple3[Int, Int, Int].compare((x1.depth, x1.index, x1.arity), (y1.depth, y1.index, y1.arity))
        case (x1: SymName.NamedSymbol, y1: SymName.LambdaParamName) =>
          Ordering.String.compare(x1.name, asStr(y1))
        case (x1: SymName.LambdaParamName, y1: SymName.NamedSymbol) =>
          Ordering.String.compare(asStr(x1), y1.name)
      }
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
