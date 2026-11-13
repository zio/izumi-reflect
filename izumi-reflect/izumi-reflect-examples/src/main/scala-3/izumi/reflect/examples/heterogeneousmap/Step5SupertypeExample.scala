//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file TypedMap.scala
// Run from this folder:  scala-cli run Step5SupertypeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.heterogeneousmap.Step5SupertypeExample"

package izumi.reflect.examples.heterogeneousmap

import izumi.reflect.Tag

/**
 * Build a Type-Safe Heterogeneous Map — Looking Up by Supertype
 *
 * Finds all values whose type is a subtype of the requested type, starting from the settings stored in "Storing Values by Exact Type".
 */
object Step5SupertypeExample {
  implicit final class TypedMapSubtypes(private val map: TypedMap) {
    def getAll[A: Tag]: List[A] =
      map.entries.collect {
        case (key, value) if key <:< Tag[A].tag => value.asInstanceOf[A]
      }.toList
  }

  def main(args: Array[String]): Unit = {
    // Setup from "Storing Values by Exact Type": three settings stored by exact type
    val settings = TypedMap.empty
      .put(List(1, 2, 3))
      .put(List("a", "b"))
      .put(Map("timeout" -> 30))

    println(s"getAll[Seq[Any]]: ${settings.getAll[Seq[Any]]}")
    println(s"getAll[Iterable[Int]]: ${settings.getAll[Iterable[Int]]}")
  }
}
