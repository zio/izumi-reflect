//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file StoredTypes.scala
// Run from this folder:  scala-cli run Step4ReadingBackExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.Step4ReadingBackExample"

package izumi.reflect.examples.storeandexchangetypetags

import izumi.reflect.Tag
import StoredTypes.{load, store}

/**
  * Store and Exchange Type Tags — Reading a Tag Back
  *
  * Reads the stored record of "Turning a Tag into Text" back into a LightTypeTag and compares it.
  */
object Step4ReadingBackExample {
  def main(args: Array[String]): Unit = {
    // Setup from "Turning a Tag into Text": the type of a Map[String, List[Int]], stored as text
    val stored = store(Tag[Map[String, List[Int]]].tag)

    val loaded = load(stored)

    println(s"loaded: $loaded")
    println(s"loaded == Tag[Map[String, List[Int]]].tag: ${loaded == Tag[Map[String, List[Int]]].tag}")
    println(s"loaded == Tag[Map[String, List[String]]].tag: ${loaded == Tag[Map[String, List[String]]].tag}")
    println(s"loaded <:< Tag[Map[String, Iterable[Int]]].tag: ${loaded <:< Tag[Map[String, Iterable[Int]]].tag}")
  }
}
