//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step3CompareTypesExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.gettingstarted.Step3CompareTypesExample"

package izumi.reflect.examples.gettingstarted

import izumi.reflect.Tag

/**
  * Getting Started with izumi-reflect — Compare Types at Runtime
  *
  * Compares tags for equality and checks subtyping, variance included.
  */
object Step3CompareTypesExample {
  def main(args: Array[String]): Unit = {
    println(s"List[Int] =:= immutable.List[Int]: ${Tag[List[Int]] =:= Tag[scala.collection.immutable.List[Int]]}")
    println(s"List[Int] <:< Seq[Any]: ${Tag[List[Int]] <:< Tag[Seq[Any]]}")
    println(s"Seq[Int] <:< List[Int]: ${Tag[Seq[Int]] <:< Tag[List[Int]]}")
  }
}
