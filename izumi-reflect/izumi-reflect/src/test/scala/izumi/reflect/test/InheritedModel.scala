package izumi.reflect.test

trait InheritedModel {

  class Dep

  trait BIOService[F[_, _]]
  type BIOServiceL[F[+_, +_], E, A] = BIOService[λ[(X, Y) => F[A, E]]]

  type F2To3[F[_, _], R, E, A] = F[E, A]

}
