package izumi.reflect.internal

import scala.collection.immutable.SortedSet
import scala.collection.mutable

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqDerivedOrdering(ordering)
  }
  @inline private[reflect] def arrayOrdering[A](ordering: Ordering[A]): Ordering[Array[A]] = {
    Ordering.Implicits.seqDerivedOrdering[mutable.IndexedSeq, A](ordering).on(array => array: mutable.IndexedSeq[A])
  }
  @inline private[reflect] def sortedSetOrdering[A](ordering: Ordering[A]): Ordering[SortedSet[A]] = {
    Ordering.Iterable(ordering).asInstanceOf[Ordering[SortedSet[A]]]
  }
  private[reflect] def setToSortedSet[A](ord: Ordering[_ >: A])(set: Set[_ <: A]): SortedSet[A] = {
    SortedSet.newBuilder(ord.asInstanceOf[Ordering[A]]).++=(set).result()
  }
}
