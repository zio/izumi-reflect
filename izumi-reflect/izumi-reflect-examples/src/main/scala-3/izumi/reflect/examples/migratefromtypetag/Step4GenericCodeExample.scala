//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step4GenericCodeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.migratefromtypetag.Step4GenericCodeExample"

package izumi.reflect.examples.migratefromtypetag

import izumi.reflect.Tag

/**
  * Migrate from scala-reflect TypeTag — Converting Generic Code
  *
  * Generic code that used TypeTag, rewritten with Tag context bounds.
  */
object Step4GenericCodeExample {
  def describe[A: Tag](value: A): String =
    s"$value: ${Tag[A].tag}"

  def accepts[A: Tag, B: Tag]: Boolean = Tag[A] <:< Tag[B]

  def main(args: Array[String]): Unit = {
    println(describe(Map("a" -> List(1, 2))))
    println(s"accepts[List[Int], Seq[Any]]: ${accepts[List[Int], Seq[Any]]}")
    println(s"accepts[Seq[Any], List[Int]]: ${accepts[Seq[Any], List[Int]]}")
  }
}
