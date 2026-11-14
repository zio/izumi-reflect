//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file TypedMap.scala
// Run from this folder:  scala-cli run Step4GenericCodeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.heterogeneousmap.Step4GenericCodeExample"

package izumi.reflect.examples.heterogeneousmap

import izumi.reflect.{Tag, TagK}

/**
  * Build a Type-Safe Heterogeneous Map — Using the Map in Generic Code
  *
  * Stores values through generic methods with Tag and TagK context bounds, using the TypedMap from the previous section.
  */
object Step4GenericCodeExample {
  def putAll[A: Tag](map: TypedMap, values: List[A]): TypedMap =
    map.put(values).put(values.headOption)

  def putGreeting[F[_]: TagK](map: TypedMap, greeting: F[String]): TypedMap =
    map.put(greeting)

  def main(args: Array[String]): Unit = {
    val withScores = putAll(TypedMap.empty, List(9.5, 7.0))

    println(s"get[List[Double]]: ${withScores.get[List[Double]]}")
    println(s"get[Option[Double]]: ${withScores.get[Option[Double]]}")
    println(s"get[Option[String]]: ${putGreeting[Option](TypedMap.empty, Some("hello")).get[Option[String]]}")
  }
}
