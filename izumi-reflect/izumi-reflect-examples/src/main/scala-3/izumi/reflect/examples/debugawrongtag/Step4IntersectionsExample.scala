//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file Model.scala
// Run from this folder:  scala-cli run Step4IntersectionsExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.Step4IntersectionsExample"

package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

/**
 * Debug a Wrong Tag or Subtype Check — Simplify Intersections Before Comparing
 *
 * A tag of Child with Parent built in generic code is not equal to the tag of Child until its tautologies are removed.
 */
object Step4IntersectionsExample {
  def withParent[T: Tag]: LightTypeTag = Tag[T with Parent].tag

  def main(args: Array[String]): Unit = {
    println(s"withParent[Child]: ${withParent[Child]}")
    println(s"withParent[Child] == Tag[Child].tag: ${withParent[Child] == Tag[Child].tag}")
    println(s"after removeIntersectionTautologies: ${withParent[Child].removeIntersectionTautologies == Tag[Child].tag.removeIntersectionTautologies}")
  }
}
