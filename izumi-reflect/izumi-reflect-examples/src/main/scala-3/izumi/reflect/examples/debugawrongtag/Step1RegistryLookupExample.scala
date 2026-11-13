//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file Model.scala
// Run from this folder:  scala-cli run Step1RegistryLookupExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step1RegistryLookupExample"

package izumi.reflect.examples.debugawrongtag



/**
 * Debug a Wrong Tag or Subtype Check — The Problem
 *
 * A value stored under the tag of its inferred type is not found by the type the caller expects.
 */
object Step1RegistryLookupExample {
  def main(args: Array[String]): Unit = {
    val registry = Registry(Map.empty).put(Some(42))

    println(s"registry.get[Option[Int]]: ${registry.get[Option[Int]]}")
  }
}
