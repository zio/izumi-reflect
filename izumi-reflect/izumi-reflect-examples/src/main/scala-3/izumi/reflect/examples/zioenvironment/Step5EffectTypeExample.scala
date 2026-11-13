//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using dep dev.zio::zio:2.1.27
//> using file Runner.scala
// Run from this folder:  scala-cli run Step5EffectTypeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.zioenvironment.Step5EffectTypeExample"

package izumi.reflect.examples.zioenvironment

import zio._
import Runner.run

/**
  * Use izumi-reflect Tags with the ZIO Environment — Writing Services Generic in an Effect Type
  *
  * A tagless-final repository in the ZIO environment, read through a generic accessor with a TagK context bound.
  */
object Step5EffectTypeExample {
  trait Repository[F[_]] {
    def find(id: Int): F[String]
  }

  def repositoryLayer[F[_]: TagK](repo: Repository[F]): ULayer[Repository[F]] =
    ZLayer.succeed(repo)

  def findWith[F[_]: TagK](id: Int): URIO[Repository[F], F[String]] =
    ZIO.serviceWith[Repository[F]](_.find(id))

  def main(args: Array[String]): Unit = {
    val optionRepo: Repository[Option] = new Repository[Option] {
      def find(id: Int): Option[String] = if (id == 1) Some("Alice") else None
    }

    println(s"findWith[Option](1): ${run(findWith[Option](1).provide(repositoryLayer(optionRepo)))}")
  }
}
