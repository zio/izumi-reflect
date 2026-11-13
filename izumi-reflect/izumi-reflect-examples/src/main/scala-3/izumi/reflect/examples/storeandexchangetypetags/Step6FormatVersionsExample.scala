//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file StoredTypes.scala
// Run from this folder:  scala-cli run Step6FormatVersionsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.Step6FormatVersionsExample"

package izumi.reflect.examples.storeandexchangetypetags

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import StoredTypes.{load, store}

/**
  * Store and Exchange Type Tags — Handling Format Versions
  *
  * Reads a stored tag with an explicit LinkageError check, and shows the error for a tag from a newer format version.
  */
object Step6FormatVersionsExample {
  def loadChecked(stored: StoredType): Either[String, LightTypeTag] =
    try Right(load(stored))
    catch {
      case e: LinkageError => Left(s"cannot read type tag: ${e.getMessage.linesIterator.next()}")
    }

  def main(args: Array[String]): Unit = {
    // Setup from "Turning a Tag into Text": the type of a Map[String, List[Int]], stored as text
    val stored = store(Tag[Map[String, List[Int]]].tag)

    println(s"currentBinaryFormatVersion: ${LightTypeTag.currentBinaryFormatVersion}")
    println(s"loadChecked(stored): ${loadChecked(stored)}")
    println(s"loadChecked(newer version): ${loadChecked(stored.copy(version = LightTypeTag.currentBinaryFormatVersion + 1))}")
  }
}
