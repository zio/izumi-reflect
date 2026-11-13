//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run InspectTagExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugging.InspectTagExample"

package izumi.reflect.examples.debugging

import izumi.reflect.{DebugProperties, Tag}

/**
 * Debugging and DebugProperties — Inspecting a Tag
 *
 * Prints the name of the debug property and the inheritance data of a tag.
 */
object InspectTagExample {
  def main(args: Array[String]): Unit = {
    println(s"debug property: ${DebugProperties.`izumi.reflect.debug.macro.rtti`}")
    println(Tag[Some[Int]].tag.debug("someInt"))
  }
}
