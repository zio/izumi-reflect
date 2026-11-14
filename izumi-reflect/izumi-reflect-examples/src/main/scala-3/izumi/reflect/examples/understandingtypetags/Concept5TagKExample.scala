//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept5TagKExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept5TagKExample"

package izumi.reflect.examples.understandingtypetags

import izumi.reflect.{Tag, TagK}

/**
  * Understanding Type Tags — 5. Type Constructors and TagK
  *
  * Renders the tag of a type constructor, applies it with combine, and builds Tag[F[A]] from TagK[F] and Tag[A].
  */
object Concept5TagKExample {
  def boxOf[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

  def main(args: Array[String]): Unit = {
    println(s"TagK[Option].tag: ${TagK[Option].tag}")
    println(s"TagK[Option].tag.combine(Tag[Int].tag): ${TagK[Option].tag.combine(Tag[Int].tag)}")
    println(s"boxOf[Vector, String]: ${boxOf[Vector, String]}")
  }
}
