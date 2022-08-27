package izumi.reflect.test

trait InheritedModel {

  class Dep

  trait BIOService[F[_, _]]
  type BIOServiceL[F[+_, +_], E, A] = BIOService[λ[(X, Y) => F[A, E]]]

  type F2To3[F[_, _], R, E, A] = F[E, A]

  type EitherR[-_, +L, +R] = Either[L, R]
  type EitherRSwap[-_, +L, +R] = Either[R, L]

  trait BlockingIO3[F[_, _, _]]
  type BlockingIO[F[_, _]] = BlockingIO3[λ[(R, E, A) => F[E, A]]]

  trait BlockingIO3T[F[_, _[_], _]]
  type BlockingIOT[F[_[_], _]] = BlockingIO3T[({ type l[R, E[_], A] = F[E, A] })#l]

  type Swap[A, B] = Either[B, A]
  type SwapF2[F[_, _], A, B] = F[B, A]

}
