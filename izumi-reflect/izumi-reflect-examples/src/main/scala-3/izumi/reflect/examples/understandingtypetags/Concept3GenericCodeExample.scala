//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept3GenericCodeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept3GenericCodeExample"

package izumi.reflect.examples.understandingtypetags

import izumi.reflect.Tag

/**
 * Understanding Type Tags — 3. Passing Tags into Generic Code
 *
 * Passes the caller's tag into a generic method with a context bound.
 */
object Concept3GenericCodeExample {
  def describe[A: Tag](value: A): String =
    s"$value has type ${Tag[A].tag}"

  def main(args: Array[String]): Unit = {
    println(describe(42))
    println(describe(List(Some("hello"))))
  }
}
