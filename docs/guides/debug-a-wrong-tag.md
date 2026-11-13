---
id: debug-a-wrong-tag
title: "Debug a Wrong Tag or Subtype Check"
description: "Find out why two izumi-reflect tags are not equal, or why a subtype check returns an unexpected result."
keywords:
  - "Debugging"
  - "Type Equality"
  - "Subtype Check"
  - "LightTypeTag"
  - "DebugProperties"
---

In this guide, we work through the steps to find out why a tag lookup fails or why `<:<` returns an unexpected result. We start with the checks that explain most problems, printing and comparing the tags, then look at the inheritance data inside a tag, and finally turn on the library's debug log. At the end, you either have a fix or the information needed for a bug report.

## The Problem

A program that keys values by `Tag` fails in a confusing way when two tags that should match don't: a service is "not found" although it was registered, or a subtype check says `false` for related types. The tags print as short names, so the difference often does not show.

For example, consider a registry that stores a value under the tag of its inferred type and looks it up by the type the caller expects:

```scala mdoc:silent
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

final case class Registry(entries: Map[LightTypeTag, Any]) {
  def put[A: Tag](value: A): Registry = Registry(entries + (Tag[A].tag -> value))
  def get[A: Tag]: Option[A] = entries.get(Tag[A].tag).map(_.asInstanceOf[A])
}

val registry = Registry(Map.empty).put(Some(42))
```

The value is there, but the lookup finds nothing:

```scala mdoc
registry.get[Option[Int]]
```

## Compare the Full Renderings

The first step is always to print both tags in full. `LightTypeTag#toString` omits packages, so two different types with the same short name look the same. `LightTypeTag#repr` shows fully qualified names and the variance of each type argument:

```scala mdoc
registry.entries.keys.map(_.repr)
Tag[Option[Int]].tag.repr
```

The stored key is `Some[Int]`, not `Option[Int]`: Scala inferred the most specific type of `Some(42)`, and `put` stored the value under it. Most mismatches are of this kind. When you compare the two renderings, look for these differences:

- A more specific inferred type, as here. Fix it by writing the type argument at the call site: `put[Option[Int]](Some(42))`.
- A different package or prefix, for example two classes named `Config` in different packages, or a type nested in a different instance of the same class.
- A different type argument, such as `Int` and `Integer`, or an abstract type member that resolved differently.
- An extra member of an intersection, such as `A with B` against `A`.

<details>
  <summary>Runnable version, with the registry from "The Problem": <code>Step2CompareRenderingsExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag/Step2CompareRenderingsExample.scala:show-line-numbers
```

</details>

## Choose Between Equality and Subtyping

A `Map` lookup by tag needs an exact match. If values of more specific types should be found too, the lookup must use a subtype check instead of equality. With the registry above:

```scala mdoc
registry.entries.keys.exists(_ <:< Tag[Option[Int]].tag)
```

Decide which one the code needs. An exact lookup is faster; a subtype lookup is more forgiving. [Build a Type-Safe Heterogeneous Map](heterogeneous-map.md#looking-up-by-supertype) shows both.

<details>
  <summary>Runnable version, with the registry from "The Problem": <code>Step3EqualityVsSubtypingExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag/Step3EqualityVsSubtypingExample.scala:show-line-numbers
```

</details>

## Simplify Intersections Before Comparing

Tags of intersection types can carry members that don't change the type, for example `Child with Parent` when `Child` already extends `Parent`. Whether such a member is kept depends on how the tag was built and on the Scala version. Two tags for the same intersection can then compare as not equal:

```scala mdoc:silent
trait Parent
trait Child extends Parent

def withParent[T: Tag]: LightTypeTag = Tag[T with Parent].tag
```

The tag built in generic code keeps `Parent`, so it is not equal to a tag of `Child` alone:

```scala mdoc
withParent[Child]
withParent[Child] == Tag[Child].tag
```

`LightTypeTag#removeIntersectionTautologies` removes such members. Apply it to both tags before comparing them:

```scala mdoc
withParent[Child].removeIntersectionTautologies == Tag[Child].tag.removeIntersectionTautologies
```

<details>
  <summary>Runnable version, with the traits and both comparisons in one program: <code>Step4IntersectionsExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag/Step4IntersectionsExample.scala:show-line-numbers
```

</details>

## Inspect the Inheritance Data

If two types print as you expect but `<:<` still gives the wrong answer, look at the inheritance data stored in the tag. `LightTypeTag#debug` prints it:

```scala mdoc
println(Tag[Child].tag.debug("child"))
```

`<:<` only knows the parents listed in this output. The tag of a type contains the parents of every type mentioned in it, as seen when the tag was created at compile time. If a parent is missing, the subtype check can't see the relationship.

<details>
  <summary>Runnable version, with the traits from "Simplify Intersections Before Comparing": <code>Step5InheritanceDataExample.scala</code></summary>

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag/Step5InheritanceDataExample.scala:show-line-numbers
```

</details>

## Check the Known Limitations

Some answers differ from the compiler on purpose, because the model of the type system in `izumi-reflect` is simplified. For example, `Null` is treated as a subtype of every type, and every type as a subtype of `AnyRef`:

```scala mdoc
Tag[Null].tag <:< Tag[String].tag
Tag[Int].tag <:< Tag[AnyRef].tag
```

Before digging deeper, compare your case with the [Limitations](../index.md#limitations) section of the introduction and with the version-specific gaps in [Scala 2 vs Scala 3](../reference/scala-2-vs-scala-3.md#known-version-specific-gaps). F-bounded types, existential types and path-dependent types are the usual suspects.

## Turn On the Debug Log

When the cause is still unclear, ask the library to explain its subtype checks. Set the system property `izumi.reflect.debug.macro.rtti` to `true` in the JVM that runs the check, for example in the forked test JVM:

```scala
Test / javaOptions += "-Dizumi.reflect.debug.macro.rtti=true"
```

Every call to `<:<` then prints the steps of the check to standard output: the two types, each parent it tries, and the result of each step. Run only the failing check with the property set, because the log is long.

On Scala 2, the same property also prints how the macro builds each tag during compilation, which helps when a tag is built from the wrong parts. Set it in the sbt JVM:

```bash
sbt -Dizumi.reflect.debug.macro.rtti=true compile
```

On Scala 3, the macros have no debug output. See [Debugging and DebugProperties](../reference/debugging.md) for all settings, including `izumi.reflect.rtti.optimized.equals`, which helps when two tags created by different versions of `izumi-reflect` compare as not equal.

## Running the Examples

All runnable code from this guide is in [`izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag/`](https://github.com/zio/izumi-reflect/tree/develop/izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/debugawrongtag). Run any file from that folder with `scala-cli run <File>.scala`, or from the repository root with `sbt "izumi-reflect-examples/runMain izumi.reflect.examples.debugawrongtag.<ObjectName>"`.

## Report a Bug

If the result is still wrong and not covered by the limitations, report it in the [izumi-reflect issue tracker](https://github.com/zio/izumi-reflect/issues). A good report contains:

- The Scala version and the `izumi-reflect` version.
- The smallest code that shows the wrong result, with the expected and actual values.
- The output of `LightTypeTag#debug` for each tag involved.
- On Scala 2, the compile-time debug log for the tags, if the problem is in how a tag is built.
