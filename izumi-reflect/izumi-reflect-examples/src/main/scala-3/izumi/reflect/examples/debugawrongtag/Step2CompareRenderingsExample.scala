//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file Model.scala
// Run from this folder:  scala-cli run Step2CompareRenderingsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step2CompareRenderingsExample"

package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag

/**
 * Debug a Wrong Tag or Subtype Check — Compare the Full Renderings
 *
 * Prints the stored and the expected tag in full, starting from the registry of "The Problem".
 */
object Step2CompareRenderingsExample {
  def main(args: Array[String]): Unit = {
    // Setup from "The Problem": Some(42) stored under the tag of its inferred type
    val registry = Registry(Map.empty).put(Some(42))

    println(s"stored keys: ${registry.entries.keys.map(_.repr)}")
    println(s"expected: ${Tag[Option[Int]].tag.repr}")
  }
}
