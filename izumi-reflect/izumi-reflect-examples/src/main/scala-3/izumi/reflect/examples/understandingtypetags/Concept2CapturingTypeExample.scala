//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Concept2CapturingTypeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.understandingtypetags.Concept2CapturingTypeExample"

package izumi.reflect.examples.understandingtypetags

import izumi.reflect.Tag

/**
  * Understanding Type Tags — 2. Capturing a Type with Tag
  *
  * Creates tags for List[Int] and List[String] and shows that they differ.
  */
object Concept2CapturingTypeExample {
  def main(args: Array[String]): Unit = {
    val numbersType = Tag[List[Int]]
    val wordsType = Tag[List[String]]

    println(s"numbersType: $numbersType")
    println(s"wordsType: $wordsType")
    println(s"numbersType == wordsType: ${numbersType == wordsType}")
    println(s"Tag[Map[String, Int]]: ${Tag[Map[String, Int]]}")
  }
}
