package izumi.reflect.internal

import scala.collection.immutable.SortedSet

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqDerivedOrdering(ordering)
  }
  @inline private[reflect] def sortedSetOrdering[A](ordering: Ordering[A]): Ordering[scala.collection.immutable.SortedSet[A]] = {
    Ordering.Iterable(ordering).asInstanceOf[Ordering[scala.collection.immutable.SortedSet[A]]]
  }
  private[reflect] def setToSortedSet[A](ord: Ordering[_ >: A])(set: Set[_ <: A]): SortedSet[A] = {
    SortedSet.newBuilder(ord.asInstanceOf[Ordering[A]]).++=(set).result()
  }
}
