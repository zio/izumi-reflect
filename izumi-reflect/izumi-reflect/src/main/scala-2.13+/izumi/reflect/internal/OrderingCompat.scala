package izumi.reflect.internal

import scala.collection.mutable

private[reflect] object OrderingCompat {
  @inline private[reflect] def listOrdering[A](ordering: Ordering[A]): Ordering[List[A]] = {
    Ordering.Implicits.seqOrdering(ordering)
  }
  @inline private[reflect] def arrayOrdering[A](ordering: Ordering[A]): Ordering[Array[A]] = {
    Ordering.Implicits.seqOrdering[ArraySeqLike, A](ordering).on(array => array)
  }
  private[reflect] final type ArraySeqLike[A] = mutable.ArraySeq[A]
}
