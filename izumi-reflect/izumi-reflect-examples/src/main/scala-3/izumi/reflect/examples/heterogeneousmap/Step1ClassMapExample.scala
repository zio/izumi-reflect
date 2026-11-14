//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step1ClassMapExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.heterogeneousmap.Step1ClassMapExample"

package izumi.reflect.examples.heterogeneousmap

import scala.reflect.ClassTag

/**
  * Build a Type-Safe Heterogeneous Map — The Problem
  *
  * A map keyed by runtime class: the second List replaces the first because both share the class List.
  */
object Step1ClassMapExample {
  final case class ClassMap(entries: Map[Class[_], Any]) {
    def put[A](value: A)(implicit ct: ClassTag[A]): ClassMap =
      ClassMap(entries + (ct.runtimeClass -> value))

    def get[A](implicit ct: ClassTag[A]): Option[A] =
      entries.get(ct.runtimeClass).map(_.asInstanceOf[A])
  }

  def main(args: Array[String]): Unit = {
    val classMap = ClassMap(Map.empty)
      .put(List(1, 2, 3))
      .put(List("a", "b"))

    println(s"classMap.get[List[Int]]: ${classMap.get[List[Int]]}")
  }
}
