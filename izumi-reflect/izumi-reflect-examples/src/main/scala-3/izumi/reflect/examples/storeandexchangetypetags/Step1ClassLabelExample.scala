//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step1ClassLabelExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.Step1ClassLabelExample"

package izumi.reflect.examples.storeandexchangetypetags

/**
  * Store and Exchange Type Tags — The Problem
  *
  * Labels stored values with their class names, which loses the type arguments.
  */
object Step1ClassLabelExample {
  def classLabel(value: Any): String = value.getClass.getName

  def main(args: Array[String]): Unit = {
    println(s"classLabel of a List[Int]: ${classLabel(List(1, 2, 3))}")
    println(s"classLabel of a List[String]: ${classLabel(List("a", "b"))}")
    println(s"classLabel of a Map[String, Int]: ${classLabel(Map("retries" -> 3))}")
  }
}
