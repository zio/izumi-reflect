//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
// Run from this folder:  scala-cli run Step6TypeKeyedMapExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.migratefromtypetag.Step6TypeKeyedMapExample"

package izumi.reflect.examples.migratefromtypetag

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

/**
 * Migrate from scala-reflect TypeTag — Converting Type-Keyed Maps
 *
 * A handler registry keyed by LightTypeTag, with lookups by supertype.
 */
object Step6TypeKeyedMapExample {
  final case class Handlers(handlers: Map[LightTypeTag, Any => Unit]) {
    def register[A: Tag](handler: A => Unit): Handlers =
      Handlers(handlers + (Tag[A].tag -> handler.asInstanceOf[Any => Unit]))

    def handlersFor[A: Tag]: List[Any => Unit] =
      handlers.collect { case (tag, h) if Tag[A].tag <:< tag => h }.toList
  }

  def main(args: Array[String]): Unit = {
    val handlers = Handlers(Map.empty)
      .register[Seq[Int]](seq => println(s"seq of ${seq.size}"))
      .register[String](s => println(s"string $s"))

    handlers.handlersFor[List[Int]].foreach(_(List(1, 2, 3)))
  }
}
