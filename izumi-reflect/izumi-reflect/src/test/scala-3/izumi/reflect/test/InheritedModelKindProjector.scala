package izumi.reflect.test

trait InheritedModelKindProjector extends InheritedModel {

  trait BIOService[F[_, _]]
  type BIOServiceL[F[+_, +_], E, A] = BIOService[[X, Y] =>> F[A, E]]

}
