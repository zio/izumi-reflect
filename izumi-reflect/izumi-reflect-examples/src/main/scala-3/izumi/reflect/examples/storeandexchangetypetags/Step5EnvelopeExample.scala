//> using scala 3.3.8
//> using dep dev.zio::izumi-reflect:3.0.11
//> using file StoredTypes.scala
// Run from this folder:  scala-cli run Step5EnvelopeExample.scala
// Or from the repository root:  sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.Step5EnvelopeExample"

package izumi.reflect.examples.storeandexchangetypetags

import izumi.reflect.Tag
import StoredTypes.{load, store}

/**
 * Store and Exchange Type Tags — Checking Stored Values Against an Expected Type
 *
 * Seals a value with its stored type and opens it as a matching and a non-matching type, using store and load from the previous sections.
 */
object Step5EnvelopeExample {
  final case class Envelope(payload: Any, payloadType: StoredType)

  def seal[A: Tag](payload: A): Envelope = Envelope(payload, store(Tag[A].tag))

  def open[A: Tag](envelope: Envelope): Either[String, A] = {
    val actual = load(envelope.payloadType)
    if (actual <:< Tag[A].tag) Right(envelope.payload.asInstanceOf[A])
    else Left(s"expected ${Tag[A].tag}, but the envelope holds $actual")
  }

  def main(args: Array[String]): Unit = {
    val envelope = seal(List(1, 2, 3))

    println(s"open[Seq[Int]]: ${open[Seq[Int]](envelope)}")
    println(s"open[List[String]]: ${open[List[String]](envelope)}")
  }
}
