package izumi.reflect.test

import izumi.reflect.macrortti._

class LambdaRenderingTest extends org.scalatest.funsuite.AnyFunSuite {
  type AB[A, B] = Either[A, B]
  type BA[A, B] = Either[B, A]

  val ab: LightTypeTag = LTT[AB[_, _]]
  val ba: LightTypeTag = `LTT[_,_]`[BA]

  test("Lambda rendering should simplify symmetric lambdas") {
    println(s"AB styled: ${ab.scalaStyledName}")
    assert(ab.scalaStyledName == "scala.util.Either[+?,+?]")
  }

  test("Lambda rendering should preserve non-symmetric lambdas") {
    println(s"BA styled: ${ba.scalaStyledName}")
    assert(ba.scalaStyledName == "[_, _] =>> scala.util.Either[+_,+_]")
  }
}
