package izumi.reflect.internal

import scala.collection.mutable

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqDerivedOrdering(ordering)
  }
  @inline private[reflect] def arrayOrdering[A](ordering: Ordering[A]): Ordering[Array[A]] = {
    Ordering.Implicits.seqDerivedOrdering[ArraySeqLike, A](ordering).on(array => array)
  }
  final type ArraySeqLike[A] = mutable.WrappedArray[A]
}
