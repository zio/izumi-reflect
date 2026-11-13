//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file Model.scala
// Run from this folder:  scala-cli run Step5InheritanceDataExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step5InheritanceDataExample"

package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag

/**
 * Debug a Wrong Tag or Subtype Check — Inspect the Inheritance Data
 *
 * Prints the inheritance data of the tag of Child, the trait defined in "Simplify Intersections Before Comparing".
 */
object Step5InheritanceDataExample {
  def main(args: Array[String]): Unit = {
    println(Tag[Child].tag.debug("child"))
  }
}
