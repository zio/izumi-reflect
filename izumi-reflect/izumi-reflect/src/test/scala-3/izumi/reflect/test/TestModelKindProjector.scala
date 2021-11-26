package izumi.reflect.test

import izumi.reflect.test.TestModel.BlockingIO3

trait TestModelKindProjector {
  type BlockingIO[F[_, _]] = BlockingIO3[[R, E, A] =>> F[E, A]]
}
