//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept1TypesDisappearExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept1TypesDisappearExample"

package izumi.reflect.examples.understandingtypetags

/**
 * Understanding Type Tags — 1. Types Disappear at Runtime
 *
 * Shows that List[Int] and List[String] share one runtime class.
 */
object Concept1TypesDisappearExample {
  def main(args: Array[String]): Unit = {
    val numbers = List(1, 2, 3)
    val words = List("a", "b")

    println(s"numbers.getClass == words.getClass: ${numbers.getClass == words.getClass}")
    println(s"numbers.getClass.getName: ${numbers.getClass.getName}")
  }
}
