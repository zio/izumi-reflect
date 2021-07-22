package izumi.reflect.internal

import scala.collection.immutable.SortedSet

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqOrdering(ordering)
  }
  @inline private[reflect] def sortedSetOrdering[A](ordering: Ordering[A]): Ordering[scala.collection.immutable.SortedSet[A]] = {
    Ordering.Implicits.sortedSetOrdering(ordering)
  }
  @inline private[reflect] def setToSortedSet[A](ord: Ordering[? >: A])(set: Set[? <: A]): SortedSet[A] = {
    SortedSet.from[A](set)(ord.asInstanceOf[Ordering[A]])
  }
}
