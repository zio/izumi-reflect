//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run BasicUsage.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.tag.BasicUsage"

package izumi.reflect.examples.tag

import izumi.reflect.{Tag, TagK}

/**
 * Tag and the TagK Family — Usage
 *
 * Asks for tags with context bounds, builds bigger tags from them, and compares tags by type and by subtyping.
 */
object BasicUsage {
  def keyOf[A: Tag]: String = Tag[A].tag.toString
  def containerOf[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

  def main(args: Array[String]): Unit = {
    println(s"keyOf[Map[String, Int]]: ${keyOf[Map[String, Int]]}")
    println(s"containerOf[Option, Int] =:= Tag[Option[Int]]: ${containerOf[Option, Int] =:= Tag[Option[Int]]}")
    println(s"containerOf[List, Int] <:< Tag[Seq[Any]]: ${containerOf[List, Int] <:< Tag[Seq[Any]]}")
  }
}
