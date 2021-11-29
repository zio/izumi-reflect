package izumi.reflect.internal

import scala.collection.immutable.SortedSet
import scala.collection.mutable

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqOrdering(ordering)
  }
  @inline private[reflect] def arrayOrdering[A](ordering: Ordering[A]): Ordering[Array[A]] = {
    Ordering.Implicits.seqOrdering[mutable.ArraySeq, A](ordering).on(array => array)
  }
//  @inline private[reflect] def sortedSetOrdering[A](ordering: Ordering[A]): Ordering[scala.collection.immutable.SortedSet[A]] = {
//    Ordering.Implicits.sortedSetOrdering(ordering)
//  }
  @deprecated("xyaa", "asdfs")
  @inline private[reflect] def setToSortedSet[A](ord: Ordering[_ >: A])(set: Set[_ <: A]): SortedSet[A] = {
    SortedSet.from[A](set)(ord.asInstanceOf[Ordering[A]])
  }
}