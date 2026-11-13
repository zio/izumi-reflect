//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step6LimitationsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step6LimitationsExample"

package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag

/**
 * Debug a Wrong Tag or Subtype Check — Check the Known Limitations
 *
 * Two subtype checks where the simplified model of izumi-reflect differs from the Scala compiler.
 */
object Step6LimitationsExample {
  def main(args: Array[String]): Unit = {
    println(s"Null <:< String: ${Tag[Null].tag <:< Tag[String].tag}")
    println(s"Int <:< AnyRef: ${Tag[Int].tag <:< Tag[AnyRef].tag}")
  }
}
