package izumi.reflect.examples.zioenvironment

import zio._

object Runner {
  def run[A](effect: ZIO[Any, Nothing, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }
}
