//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file TypedMap.scala
// Run from this folder:  scala-cli run Step3ExactTypeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.heterogeneousmap.Step3ExactTypeExample"

package izumi.reflect.examples.heterogeneousmap

import izumi.reflect.Tag

/**
 * Build a Type-Safe Heterogeneous Map — Storing Values by Exact Type
 *
 * Stores values of different types in a TypedMap and reads them back by exact type and by alias.
 */
object Step3ExactTypeExample {
  type Ports = List[Int]

  def main(args: Array[String]): Unit = {
    val settings = TypedMap.empty
      .put(List(1, 2, 3))
      .put(List("a", "b"))
      .put(Map("timeout" -> 30))

    println(s"get[List[Int]]: ${settings.get[List[Int]]}")
    println(s"get[List[String]]: ${settings.get[List[String]]}")
    println(s"get[Map[String, Int]]: ${settings.get[Map[String, Int]]}")
    println(s"get[Option[Int]]: ${settings.get[Option[Int]]}")
    println(s"get[Ports]: ${settings.get[Ports]}")
    println(s"get[immutable.List[Int]]: ${settings.get[scala.collection.immutable.List[Int]]}")
  }
}
