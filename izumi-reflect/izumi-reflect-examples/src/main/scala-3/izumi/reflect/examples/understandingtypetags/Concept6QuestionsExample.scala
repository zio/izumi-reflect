//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept6QuestionsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept6QuestionsExample"

package izumi.reflect.examples.understandingtypetags

import izumi.reflect.Tag

/**
 * Understanding Type Tags — 6. Asking Questions About Types
 *
 * Compares types with =:= and checks subtyping with <:<, variance included.
 */
object Concept6QuestionsExample {
  def main(args: Array[String]): Unit = {
    println(s"List[Int] =:= immutable.List[Int]: ${Tag[List[Int]] =:= Tag[scala.collection.immutable.List[Int]]}")
    println(s"List[Int] <:< Seq[Int]: ${Tag[List[Int]] <:< Tag[Seq[Int]]}")
    println(s"Seq[Int] <:< List[Int]: ${Tag[Seq[Int]] <:< Tag[List[Int]]}")
    println(s"List[String] <:< Seq[Any]: ${Tag[List[String]] <:< Tag[Seq[Any]]}")
  }
}
