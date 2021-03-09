package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.macrortti.LTT

class DottyRegressionTests extends TagAssertions {
  "regression tests" should {
    "support string constant types" in {
      assertDifferent(LTT["abc"], LTT[String])
      assertDifferent(LTT["abc"], LTT["cba"])
      assertChild(LTT["abc"], LTT["abc"])
      assertChild(LTT["abc"], LTT[String])
      assertNotChild(LTT[String], LTT["abc"])
    }
  }
}

object DottyRegressionTests {
  //https://github.com/zio/izumi-reflect/issues/135#issue-801046733
  import zio._

  trait DgraphClient

  object Example extends scala.App {

    type DgClient = Has[DgClient.Service]

    object DgClient {

      trait Service {
        val dgraphClient: UIO[DgraphClient]
      }

      val getDgClient =
        ZIO.accessM[DgClient](_.get.dgraphClient)
    }
  }

  object zio {
    type UIO[+A] = ZIO[Any, Nothing, A]
    sealed trait ZIO[-R, +E, +A]
    object ZIO {
      def accessM[R]: AccessMPartiallyApplied[R] = new AccessMPartiallyApplied[R]()
      final class AccessMPartiallyApplied[R](private val dummy: Boolean = false) extends AnyVal {
        def apply[E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] = ???
      }
    }
    trait Has[A]
    object Has {
      implicit final class HasSyntax[Self <: Has[_]](private val self: Self) extends AnyVal {
        def get[B](implicit ev: Self <:< Has[B], tagged: Tag[B]): B = ???
      }
    }
  }
}
