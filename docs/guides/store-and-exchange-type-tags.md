---
id: store-and-exchange-type-tags
title: "Store and Exchange Type Tags"
description: "Save izumi-reflect type tags as text, read them back in another process, and handle format versions safely."
keywords:
  - "Serialization"
  - "Event Store"
  - "Type Information"
  - "LightTypeTag"
---

In this guide, we save the full type of a value next to the value itself, so that another process, or the same program after a restart, can read it back and check it against the type it expects. We start by turning a type tag into text, then read it back and compare it, and finish with the version check that keeps old readers from misreading tags written by newer code.

## The Problem

Persisted data often needs a record of its type: an event store keeps the type of each event, a message bus labels each message, a cache stores values of different types side by side. The obvious label is the class name, but the JVM class of a value has lost its type arguments, and it is often an implementation class rather than the type the program works with.

For example, consider labelling stored values with their class names:

```scala mdoc
def classLabel(value: Any): String = value.getClass.getName

classLabel(List(1, 2, 3))
classLabel(List("a", "b"))
classLabel(Map("retries" -> 3))
```

Both lists get the label of the private list cell class, `::`, and the type arguments `Int` and `String` are gone. A reader can't tell a `List[Int]` from a `List[String]`, so a wrong cast only fails later, far from the cause. A `LightTypeTag` describes the full type, and it can be written out and read back.

## Prerequisites

Add `izumi-reflect` to your build:

```scala
libraryDependencies += "dev.zio" %% "izumi-reflect" % "@VERSION@"
```

The guide uses `LightTypeTag#serialize` and `LightTypeTag.parse`, described in [LightTypeTag](../reference/light-type-tag.md#serialization). All examples use these imports:

```scala mdoc:silent
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.util.Base64
```

## Turning a Tag into Text

`LightTypeTag#serialize` returns a `LightTypeTag.Serialized`, a case class of four fields: a hash code, the structure of the type, the inheritance data used by subtype checks, and the format version. To serialize the tag of a type:

```scala mdoc:silent
val serialized: LightTypeTag.Serialized = Tag[Map[String, List[Int]]].tag.serialize()
```

The two strings hold binary data: each character stands for one byte, in the ISO-8859-1 encoding, and many are control characters. Text columns, JSON and log lines can mangle them, so encode each string as Base64 before storing it. We keep the four fields in a small record of our own, which is what the storage layer sees:

```scala mdoc:silent
final case class StoredType(hash: Int, ref: String, databases: String, version: Int)

def toBase64(binary: String): String =
  Base64.getEncoder.encodeToString(binary.getBytes(ISO_8859_1))

def store(tag: LightTypeTag): StoredType = {
  val s = tag.serialize()
  StoredType(s.hash, toBase64(s.ref), toBase64(s.databases), s.version)
}
```

The stored record contains only printable text and two numbers. To store the type of a `Map[String, List[Int]]` and look at the result:

```scala mdoc
val stored = store(Tag[Map[String, List[Int]]].tag)
```

<details>
  <summary>Runnable version, with the serialization and the stored record in one program: <code>Step3TurningIntoTextExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags/Step3TurningIntoTextExample.scala:show-line-numbers
```

</details>

## Reading a Tag Back

To read a tag back, decode the two strings and pass the four fields to `LightTypeTag.parse`. The result is a `LightTypeTag` like any other, equal to the tag of the same type created by the compiler:

```scala mdoc:silent
def fromBase64(text: String): String =
  new String(Base64.getDecoder.decode(text), ISO_8859_1)

def load(stored: StoredType): LightTypeTag =
  LightTypeTag.parse(LightTypeTag.Serialized(stored.hash, fromBase64(stored.ref), fromBase64(stored.databases), stored.version))
```

To read the stored tag back and compare it with the tag the reader expects:

```scala mdoc
val loaded = load(stored)

loaded
loaded == Tag[Map[String, List[Int]]].tag
loaded == Tag[Map[String, List[String]]].tag
```

The loaded tag keeps the inheritance data, so subtype checks work too. A reader that accepts any `Map[String, Iterable[Int]]` can check the stored type with `LightTypeTag#<:<`:

```scala mdoc
loaded <:< Tag[Map[String, Iterable[Int]]].tag
```

<details>
  <summary>Runnable version, with the record stored in "Turning a Tag into Text": <code>Step4ReadingBackExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags/Step4ReadingBackExample.scala:show-line-numbers
```

</details>

## Checking Stored Values Against an Expected Type

With tags that survive storage, a reader can refuse values whose stored type does not match what it expects, instead of casting blindly. An envelope keeps the value next to its stored type, and the reader compares before it casts:

```scala mdoc:silent
final case class Envelope(payload: Any, payloadType: StoredType)

def seal[A: Tag](payload: A): Envelope = Envelope(payload, store(Tag[A].tag))

def open[A: Tag](envelope: Envelope): Either[String, A] = {
  val actual = load(envelope.payloadType)
  if (actual <:< Tag[A].tag) Right(envelope.payload.asInstanceOf[A])
  else Left(s"expected ${Tag[A].tag}, but the envelope holds $actual")
}
```

To seal a `List[Int]` and open it as a matching and as a non-matching type:

```scala mdoc
val envelope = seal(List(1, 2, 3))

open[Seq[Int]](envelope)
open[List[String]](envelope)
```

In a real system, the payload is serialized too, for example as JSON. The stored type then decides which decoder to use, and the check stops a reader from decoding a payload with the wrong one.

<details>
  <summary>Runnable version, with `store` and `load` from the previous sections: <code>Step5EnvelopeExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags/Step5EnvelopeExample.scala:show-line-numbers
```

</details>

## Handling Format Versions

The `version` field is the binary format version of the two strings. `LightTypeTag.parse` reads tags written by the same or an older version of `izumi-reflect`. A tag written by a newer version can't be read: `LightTypeTag.parse` throws a `java.lang.LinkageError` that asks for a newer `izumi-reflect` jar. The version the library on the classpath writes is `LightTypeTag.currentBinaryFormatVersion`.

`LinkageError` counts as a fatal error in Scala, so `scala.util.Try` does not catch it. Catch it explicitly where tags from other processes are read:

```scala mdoc:silent
def loadChecked(stored: StoredType): Either[String, LightTypeTag] =
  try Right(load(stored))
  catch {
    case e: LinkageError => Left(s"cannot read type tag: ${e.getMessage.linesIterator.next()}")
  }
```

To see what happens with a tag from a future version, we change the version of the stored record:

```scala mdoc
LightTypeTag.currentBinaryFormatVersion

loadChecked(stored)
loadChecked(stored.copy(version = LightTypeTag.currentBinaryFormatVersion + 1))
```

When services of different versions exchange tags, upgrade `izumi-reflect` in the readers before the writers. A reader on a newer version reads every older tag; a reader on an older version fails on tags from a newer one.

:::note
Tags from an older format are read without their stored hash code, so the first comparisons are a little slower. Tags written by the current format compare by their serialized strings first, which is fast.
:::

<details>
  <summary>Runnable version, with the record stored in "Turning a Tag into Text": <code>Step6FormatVersionsExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags/Step6FormatVersionsExample.scala:show-line-numbers
```

</details>

## Running the Examples

All runnable code from this guide is in [`izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags/`](https://github.com/zio/izumi-reflect/tree/develop/izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/storeandexchangetypetags). Run any file from that folder with `scala-cli run <File>.scala`, or from the repository root with `sbt "izumi-reflect-examples/runMain izumi.reflect.examples.storeandexchangetypetags.<ObjectName>"`.

## Going Further

The stored type works well as a key for a registry of decoders, or as the type label of events in an event store. For more details:

- [LightTypeTag](../reference/light-type-tag.md#serialization) covers `LightTypeTag#serialize`, `LightTypeTag.parse` and the Java serialization support of tags.
- [Build a Type-Safe Heterogeneous Map](heterogeneous-map.md) shows exact and supertype lookups by tag.
- [Debug a Wrong Tag or Subtype Check](debug-a-wrong-tag.md) helps when a stored type unexpectedly does not match.
