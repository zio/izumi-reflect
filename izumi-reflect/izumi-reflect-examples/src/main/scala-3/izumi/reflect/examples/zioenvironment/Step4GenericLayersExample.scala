//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using dep dev.zio::zio:2.1.27
//> using file Runner.scala
// Run from this folder:  scala-cli run Step4GenericLayersExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.zioenvironment.Step4GenericLayersExample"

package izumi.reflect.examples.zioenvironment

import zio._
import Runner.run

/**
  * Use izumi-reflect Tags with the ZIO Environment — Writing Generic Layers
  *
  * Provides two caches that differ only in their type arguments and reads both.
  */
object Step4GenericLayersExample {
  final case class Cache[K, V](entries: Map[K, V]) {
    def get(key: K): Option[V] = entries.get(key)
  }

  def cacheLayer[K: Tag, V: Tag](entries: (K, V)*): ULayer[Cache[K, V]] =
    ZLayer.succeed(Cache(entries.toMap))

  def main(args: Array[String]): Unit = {
    val lookups: URIO[Cache[String, Int] with Cache[Int, String], (Option[Int], Option[String])] =
      for {
        byName <- ZIO.serviceWith[Cache[String, Int]](_.get("answer"))
        byId <- ZIO.serviceWith[Cache[Int, String]](_.get(42))
      } yield (byName, byId)

    println(s"lookups: ${run(lookups.provide(cacheLayer("answer" -> 42), cacheLayer(42 -> "answer")))}")
  }
}
