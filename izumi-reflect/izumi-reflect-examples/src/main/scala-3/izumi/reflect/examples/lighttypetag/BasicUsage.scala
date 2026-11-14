//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run BasicUsage.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.lighttypetag.BasicUsage"

package izumi.reflect.examples.lighttypetag

import izumi.reflect.{Tag, TagK}

/**
  * LightTypeTag — Usage
  *
  * Renders a LightTypeTag, checks subtyping, reads its type arguments and rebuilds it with combine.
  */
object BasicUsage {
  def main(args: Array[String]): Unit = {
    val listOfInt = Tag[List[Int]].tag

    println(s"listOfInt.toString: ${listOfInt.toString}")
    println(s"listOfInt <:< Tag[Seq[Any]].tag: ${listOfInt <:< Tag[Seq[Any]].tag}")
    println(s"listOfInt.typeArgs: ${listOfInt.typeArgs}")
    println(s"TagK[List].tag.combine(Tag[Int].tag) == listOfInt: ${TagK[List].tag.combine(Tag[Int].tag) == listOfInt}")
  }
}
