//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step4GenericCodeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.gettingstarted.Step4GenericCodeExample"

package izumi.reflect.examples.gettingstarted

import izumi.reflect.{Tag, TagK}

/**
  * Getting Started with izumi-reflect — Carry Tags Through Generic Code
  *
  * Passes tags into generic methods with Tag and TagK context bounds and builds tags of bigger types from them.
  */
object Step4GenericCodeExample {
  def describe[A: Tag](value: A): String =
    s"$value has type ${Tag[A].tag}"

  def mapOf[A: Tag]: Tag[Map[String, A]] = Tag[Map[String, A]]

  def containerOf[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

  def main(args: Array[String]): Unit = {
    println(describe(42))
    println(describe(List(Some("a"), None)))
    println(s"mapOf[Double]: ${mapOf[Double]}")
    println(s"containerOf[Option, Int]: ${containerOf[Option, Int]}")
    println(s"containerOf[Vector, String]: ${containerOf[Vector, String]}")
  }
}
