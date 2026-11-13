//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run BasicUsage.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.ltag.BasicUsage"

package izumi.reflect.examples.ltag

import izumi.reflect.macrortti._

/**
 * LTag and LTT — Usage
 *
 * Builds LightTypeTags of concrete types with LTT and LTag, and a weak tag that keeps a type parameter as a placeholder.
 */
object BasicUsage {
  def weakOptionTag[T]: LightTypeTag = LTag.Weak[Option[T]].tag

  def main(args: Array[String]): Unit = {
    println(s"LTT[List[Int]]: ${LTT[List[Int]]}")
    println(s"LTag[Either[String, Int]].tag: ${LTag[Either[String, Int]].tag}")
    println(s"weakOptionTag[Int]: ${weakOptionTag[Int]}")
  }
}
