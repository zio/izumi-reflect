//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step5HigherKindedExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.migratefromtypetag.Step5HigherKindedExample"

package izumi.reflect.examples.migratefromtypetag

import izumi.reflect.{Tag, TagK}

/**
 * Migrate from scala-reflect TypeTag — Converting Higher-Kinded Code
 *
 * Asks for TagK[F] instead of extracting a type constructor from a TypeTag at runtime.
 */
object Step5HigherKindedExample {
  trait Repository[F[_]]

  def repositoryTag[F[_]: TagK]: Tag[Repository[F]] = Tag[Repository[F]]

  def main(args: Array[String]): Unit = {
    println(s"repositoryTag[Option]: ${repositoryTag[Option]}")
    println(s"TagK[List].tag: ${TagK[List].tag}")
  }
}
