//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run TwoPhasesExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.referenceoverview.TwoPhasesExample"

package izumi.reflect.examples.referenceoverview

import izumi.reflect.{Tag, TagK}

/**
 * Reference Overview — How They Work Together
 *
 * A tag built at runtime from TagK[F] and Tag[A] equals the tag the compiler creates for the concrete type.
 */
object TwoPhasesExample {
  def built[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

  def main(args: Array[String]): Unit = {
    println(s"built[List, Int]: ${built[List, Int]}")
    println(s"built[List, Int] == Tag[List[Int]]: ${built[List, Int] == Tag[List[Int]]}")
  }
}
