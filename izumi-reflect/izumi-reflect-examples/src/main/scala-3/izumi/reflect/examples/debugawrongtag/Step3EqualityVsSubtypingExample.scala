//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file Model.scala
// Run from this folder:  scala-cli run Step3EqualityVsSubtypingExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step3EqualityVsSubtypingExample"

package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag

/**
  * Debug a Wrong Tag or Subtype Check — Choose Between Equality and Subtyping
  *
  * Finds the stored value with a subtype check where the exact lookup fails, starting from the registry of "The Problem".
  */
object Step3EqualityVsSubtypingExample {
  def main(args: Array[String]): Unit = {
    // Setup from "The Problem": Some(42) stored under the tag of its inferred type
    val registry = Registry(Map.empty).put(Some(42))

    println(s"some key <:< Option[Int]: ${registry.entries.keys.exists(_ <:< Tag[Option[Int]].tag)}")
  }
}
