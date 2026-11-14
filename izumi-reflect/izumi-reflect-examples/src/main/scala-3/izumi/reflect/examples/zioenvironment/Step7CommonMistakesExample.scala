//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using dep dev.zio::zio:2.1.27
//> using file Runner.scala
// Run from this folder:  scala-cli run Step7CommonMistakesExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.zioenvironment.Step7CommonMistakesExample"

package izumi.reflect.examples.zioenvironment

import zio._
import Runner.run

/**
  * Use izumi-reflect Tags with the ZIO Environment — Common Mistakes
  *
  * Registers a FixedClock under the type its users ask for, Clock2, and reads it back as that type.
  */
object Step7CommonMistakesExample {
  // Setup from "Looking Up Services by Supertype": the service interface and its implementation
  trait Clock2 { def now: Long }
  final case class FixedClock(now: Long) extends Clock2

  def main(args: Array[String]): Unit = {
    println(s"now: ${run(ZIO.serviceWith[Clock2](_.now).provide(ZLayer.succeed[Clock2](FixedClock(1700000000L))))}")
  }
}
