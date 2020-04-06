package izumi.reflect.test

import izumi.reflect.test.TestModel.BlockingIO3

trait TestModelKindProjector {
  type BlockingIO[F[_, _]] = BlockingIO3[Lambda[(R, E, A) => F[E, A]]]
}
