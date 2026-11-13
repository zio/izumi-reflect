//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using dep dev.zio::zio:2.1.27
// Run from this folder:  scala-cli run Step6SupertypeLookupExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.zioenvironment.Step6SupertypeLookupExample"

package izumi.reflect.examples.zioenvironment

import zio._

/**
  * Use izumi-reflect Tags with the ZIO Environment — Looking Up Services by Supertype
  *
  * Stores a FixedClock in a ZEnvironment and reads it back as its supertype Clock2.
  */
object Step6SupertypeLookupExample {
  trait Clock2 { def now: Long }
  final case class FixedClock(now: Long) extends Clock2

  def main(args: Array[String]): Unit = {
    val environment: ZEnvironment[FixedClock] = ZEnvironment(FixedClock(1700000000L))

    println(s"environment.get[Clock2].now: ${environment.get[Clock2].now}")
  }
}
