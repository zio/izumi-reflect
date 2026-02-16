package izumi.reflect.internal

private[reflect] object CollectionCompat {
  private[reflect] final type IterableOnce[+A] = scala.collection.TraversableOnce[A]
}
