//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file StoredTypes.scala
// Run from this folder:  scala-cli run Step3TurningIntoTextExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.Step3TurningIntoTextExample"

package izumi.reflect.examples.storeandexchangetypetags

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import StoredTypes.store

/**
  * Store and Exchange Type Tags — Turning a Tag into Text
  *
  * Serializes a tag and stores it as a record of Base64 text and two numbers.
  */
object Step3TurningIntoTextExample {
  def main(args: Array[String]): Unit = {
    val serialized: LightTypeTag.Serialized = Tag[Map[String, List[Int]]].tag.serialize()
    println(s"serialized.version: ${serialized.version}")

    val stored = store(Tag[Map[String, List[Int]]].tag)
    println(s"stored: $stored")
  }
}
