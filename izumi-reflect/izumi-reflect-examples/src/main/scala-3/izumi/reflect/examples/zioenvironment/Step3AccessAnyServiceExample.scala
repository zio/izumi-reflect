//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using dep dev.zio::zio:2.1.27
//> using file Runner.scala
// Run from this folder:  scala-cli run Step3AccessAnyServiceExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.zioenvironment.Step3AccessAnyServiceExample"

package izumi.reflect.examples.zioenvironment

import zio._
import Runner.run

/**
  * Use izumi-reflect Tags with the ZIO Environment — Accessing Any Service
  *
  * A generic service accessor with a Tag context bound, run with the helper from Prerequisites.
  */
object Step3AccessAnyServiceExample {
  def serviceOf[A: Tag]: URIO[A, A] = ZIO.service[A]

  final case class Config(url: String)

  def main(args: Array[String]): Unit = {
    println(s"serviceOf[Config]: ${run(serviceOf[Config].provide(ZLayer.succeed(Config("jdbc:h2:mem"))))}")
  }
}
