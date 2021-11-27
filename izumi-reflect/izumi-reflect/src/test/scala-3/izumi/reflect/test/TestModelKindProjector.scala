package izumi.reflect.test

import izumi.reflect.test.TestModel._

trait TestModelKindProjector {

  type BlockingIO[F[_, _]] = BlockingIO3[[R, E, A] =>> F[E, A]]

  type BIO2[F[+_, +_]] = BIO3[[R, E, A] =>> F[E, A]]

}
