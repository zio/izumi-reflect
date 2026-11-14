//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step2FirstTagExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.gettingstarted.Step2FirstTagExample"

package izumi.reflect.examples.gettingstarted

import izumi.reflect.Tag

/**
  * Getting Started with izumi-reflect — Create Your First Tag
  *
  * Summons tags for a few concrete types and renders a LightTypeTag in short and fully qualified forms.
  */
object Step2FirstTagExample {
  def main(args: Array[String]): Unit = {
    println(s"Tag[Int]: ${Tag[Int]}")
    println(s"Tag[List[String]]: ${Tag[List[String]]}")
    println(s"Tag[Either[String, Option[Int]]]: ${Tag[Either[String, Option[Int]]]}")

    val listTag = Tag[List[String]].tag

    println(s"listTag.toString: ${listTag.toString}")
    println(s"listTag.repr: ${listTag.repr}")
    println(s"listTag.shortName: ${listTag.shortName}")
  }
}
