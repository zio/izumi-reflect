//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept4BiggerTagsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept4BiggerTagsExample"

package izumi.reflect.examples.understandingtypetags

import izumi.reflect.Tag

/**
  * Understanding Type Tags — 4. Building Bigger Tags from Smaller Ones
  *
  * Builds the tag of Map[String, A] from the tag of A.
  */
object Concept4BiggerTagsExample {
  def settingsOf[A: Tag]: Tag[Map[String, A]] = Tag[Map[String, A]]

  def main(args: Array[String]): Unit = {
    println(s"settingsOf[Boolean]: ${settingsOf[Boolean]}")
    println(s"settingsOf[List[Double]]: ${settingsOf[List[Double]]}")
  }
}
