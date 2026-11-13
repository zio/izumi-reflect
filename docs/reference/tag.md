---
id: tag
title: "Tag and the TagK Family"
sidebar_label: "Tag and TagK"
description: "Reference for Tag, TagK and the rest of the tag family: summoning, comparing, tags of any kind and building tags by hand."
keywords:
  - "Runtime Type Information"
  - "Type Class"
  - "Higher-Kinded Types"
  - "Tag"
---

`Tag[T]` is a type class whose instance describes the type `T` at runtime. You ask for it with a context bound (`def f[T: Tag]`), and the compiler creates it at compile time, combining the tags of any type parameters that are in scope. `TagK`, `TagKK` and their siblings do the same for type constructors such as `F[_]`.

A `Tag` is a thin wrapper: its `tag` field holds a [`LightTypeTag`](light-type-tag.md), which does the type comparisons and rendering, and its `AnyTag#closestClass` field holds the JVM class that best matches the type.

```scala
trait Tag[T] extends AnyTag {
  def tag: LightTypeTag
  def closestClass: Class[_]
}

object Tag {
  def apply[T: Tag]: Tag[T]
  def apply[T](cls: Class[_], tag0: LightTypeTag): Tag[T]
}

type TagK[K[_]]     // and TagKK, TagK3, TagT, TagTK, TagTKK, TagTK3, TagK4 ... TagK22
```

## Usage

A generic method asks for tags with context bounds, and the compiler builds the tags of bigger types from them. The resulting tags compare by type and check subtyping:

```scala mdoc:reset
import izumi.reflect.{Tag, TagK}

def keyOf[A: Tag]: String = Tag[A].tag.toString
def containerOf[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]

keyOf[Map[String, Int]]
containerOf[Option, Int] =:= Tag[Option[Int]]
containerOf[List, Int] <:< Tag[Seq[Any]]
```

[Build a Type-Safe Heterogeneous Map](../guides/heterogeneous-map.md) and [Use izumi-reflect Tags with the ZIO Environment](../guides/zio-environment.md) show `Tag` at work in complete programs.

## Summoning a Tag

There are three equivalent ways to get a `Tag`: `Tag.apply`, `implicitly`, or a context bound on a type parameter. To summon tags for concrete types:

```scala mdoc
import izumi.reflect._

Tag[String]
implicitly[Tag[Map[String, Int]]]
```

For concrete types, the macro computes the whole tag at compile time. For types that mention type parameters, it searches the implicit scope for a tag of each parameter and combines them, so a generic method only needs a context bound:

```scala mdoc
def tagOfPair[A: Tag, B: Tag]: Tag[(A, B)] = Tag[(A, B)]

tagOfPair[Int, List[String]]
```

If a type parameter has no tag in scope, compilation fails with a message that names the missing tag. The fix is to add a context bound for it.

The implicit the compiler finds is the macro `Tag.tagFromTagMacro` (and, on Scala 2, `HKTag.hktagFromTagMacro` for type constructors). You see these names in IDE implicit hints and at the top of macro errors; you never call them yourself.

## Members of Tag

Every `Tag` (and every other kind of tag, see [Tags for Type Constructors](#tags-for-type-constructors)) extends the `AnyTag` trait, which defines the following members:

```scala
trait AnyTag extends Serializable {
  def tag: LightTypeTag
  def closestClass: Class[_]
  final def hasPreciseClass: Boolean
  final def =:=(that: AnyTag): Boolean
  final def <:<(that: AnyTag): Boolean
}
```

### Comparing Tags

`AnyTag#=:=` checks that two tags describe the same type, and `AnyTag#<:<` checks that the left type is a subtype of the right one. Both delegate to the `LightTypeTag` in the `tag` field. To compare tags:

```scala mdoc
Tag[Some[Int]] <:< Tag[Option[Any]]
Tag[Option[Any]] <:< Tag[Some[Int]]
Tag[String] =:= Tag[java.lang.String]
```

`Tag#equals` and `Tag#hashCode` follow `=:=`: two tags are equal when their `LightTypeTag`s are equal, and `AnyTag#closestClass` plays no part. This makes `Tag` safe to use as a key in hash-based collections.

### The Closest Class

`AnyTag#closestClass` returns the JVM class that best represents the type. For a class or trait, it is that class; for a type alias, the class of the aliased type; for an intersection type, the least upper bound of its members, which is often `Object`. To see the closest class for a few types:

```scala mdoc
type Name = String

Tag[Name].closestClass
Tag[List[Int]].closestClass
Tag[java.io.Serializable with CharSequence].closestClass
```

A Scala type does not always have a matching JVM class, so `AnyTag#closestClass` is not always enough to create or inspect values of the type. `AnyTag#hasPreciseClass` tells you whether it is safe to reflect on `AnyTag#closestClass`: it returns `true` when the class name matches the type name:

```scala mdoc
Tag[Name].hasPreciseClass
Tag[java.io.Serializable with CharSequence].hasPreciseClass
```

### Serializing a Tag

`AnyTag` extends `java.io.Serializable`, so you can send a `Tag` through Java serialization, for example between nodes of a cluster. A deserialized tag is equal to the original:

```scala mdoc:silent
import java.io._

def roundTrip(value: AnyRef): AnyRef = {
  val bytes = new ByteArrayOutputStream()
  val out = new ObjectOutputStream(bytes)
  out.writeObject(value)
  out.close()
  new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray)).readObject()
}
```

Now we can serialize a tag and compare the result with a freshly summoned tag:

```scala mdoc
roundTrip(Tag[List[Int]]) == Tag[List[Int]]
```

For a compact, versioned string format that does not depend on Java serialization, use `LightTypeTag#serialize` instead. See [Serialization](light-type-tag.md#serialization).

## Tags for Type Constructors

A plain `Tag[T]` describes a proper type, such as `Int` or `List[String]`. To describe a type constructor, such as `List` or `Either`, `izumi-reflect` provides one type class per kind. Each one has a companion object with a summoner, so `TagK.apply` works like `Tag.apply`:

| Type class | Kind of the type parameter | Typical use                                      |
| ---------- | -------------------------- | ------------------------------------------------ |
| `TagK`     | `F[_]`                     | Effect types and containers: `Option`, `List`    |
| `TagKK`    | `F[_, _]`                  | Bifunctors: `Either`, `Map`                      |
| `TagK3`    | `F[_, _, _]`               | Three-parameter types: `Function2`, `ZIO`        |
| `TagT`     | `K[_[_]]`                  | Type classes over a type constructor: `Functor`  |
| `TagTK`    | `K[_[_], _]`               | Monad transformers such as `OptionT`             |
| `TagTKK`   | `K[_[_], _, _]`            | Monad transformers such as `EitherT`             |
| `TagTK3`   | `K[_[_], _, _, _]`         | Transformers with three extra type parameters    |

`TagK4` up to `TagK22` cover type constructors with 4 to 22 type parameters. They are type aliases only, without a companion object, so you summon them with `implicitly` or a context bound.

To summon tags for a few type constructors:

```scala mdoc
TagK[Option]
TagKK[Either]
TagK3[Function2]
```

The printed form shows an unapplied type constructor as a type lambda: `λ %0 → Option[+0]` reads as "the type function that takes a type `%0` and returns `Option[+%0]`".

### Type Constructors in Generic Code

The main use of `TagK` is in code that is generic over a type constructor, such as tagless-final services. With a `TagK` context bound on `F`, you can summon a tag for any type built from `F`:

```scala mdoc
trait UserRepository[F[_]]

def repositoryTag[F[_]: TagK]: Tag[UserRepository[F]] = Tag[UserRepository[F]]

repositoryTag[Option]
```

The printed name includes the `MdocSession::MdocApp` prefix because the examples on this page are compiled inside an object generated by the documentation tool. In your code, the prefix is your own package.

Partially applied type constructors work too. A type alias with one free parameter gives a `TagK`:

```scala mdoc
type Result[A] = Either[String, A]

TagK[Result]
```

### Tags of Any Kind

The fixed list of `TagK*` aliases covers the common kinds. For other kinds, the API differs between Scala 2 and Scala 3.

On Scala 3, `Tag` itself accepts a type of any kind, and all the `TagK*` names are aliases of `Tag`. So `Tag[Option]` and `TagK[Option]` are the same thing, and you can write a context bound for any kind directly:

```scala mdoc
trait Functor[F[_]]

Tag[Functor]
TagT[Functor] == Tag[Functor]
```

On Scala 2, `Tag` only accepts proper types. Each `TagK*` alias is a `HKTag` applied to a structural type that spells out the kind, for example `TagK[K]` is `HKTag[{ type Arg[A] = K[A] }]`. For a kind without a predefined alias, use `Tag.auto.T`, a macro that infers the right `HKTag` type for the kind of its argument:

```scala
// Scala 2 only
import izumi.reflect.Tag

def tagOfAnyKind[K[_[_, _], _[_], _[_[_], _, _, _]]: Tag.auto.T]: Tag.auto.T[K] =
  implicitly[Tag.auto.T[K]]
```

On Scala 3, `Tag.auto.T[X]` is a plain alias of `Tag[X]`, so cross-built code can use `Tag.auto.T` on both versions. See [Scala 2 vs Scala 3](scala-2-vs-scala-3.md) for the other differences.

## Creating Tags by Hand

You rarely create tags by hand; it is an advanced API for libraries that build types at runtime, prefer summoning tags with context bounds. The macro uses these constructors itself to combine tags of type parameters.

### Tag.apply

`Tag.apply(cls, tag)` wraps an existing `LightTypeTag` and a class into a `Tag`. Use it when you already have a `LightTypeTag`, for example one parsed from its serialized form:

```scala mdoc
import izumi.reflect.macrortti.LightTypeTag

val stringLtt: LightTypeTag = Tag[String].tag

Tag[String](classOf[String], stringLtt) == Tag[String]
```

### Tag.appliedTag

`Tag.appliedTag` applies the type constructor in a higher-kinded tag to a list of argument tags, in order:

```scala mdoc
Tag.appliedTag[List[Int]](TagK[List], List(Tag[Int].tag))
```

The result is equal to the tag the compiler creates for the same type:

```scala mdoc
Tag.appliedTag[List[Int]](TagK[List], List(Tag[Int].tag)) == Tag[List[Int]]
```

The same operation at the `LightTypeTag` level is `LightTypeTag#combine`, which also handles partial application. See [Combining Type Constructors](light-type-tag.md#combining-type-constructors).

### Refined and Union Tags

`HKTag.appliedTagNonPos` (and, on Scala 3, `Tag.appliedTagNonPos`) works like `Tag.appliedTag`, but takes an `Option` per type parameter and leaves the `None` positions open, like [`LightTypeTag#combineNonPos`](light-type-tag.md#lighttypetagcombinenonpos). `HKTag.appliedTagNonPosAux` does the same from a class and a `LightTypeTag` instead of a tag.

`Tag.refinedTag` builds the tag of an intersection type with a structural refinement, such as `A with B { def x: Int }`, from the tags of its parts. On Scala 3, `Tag.unionTag` builds the tag of a union type from the tags of its members:

```scala mdoc
Tag.unionTag[Int | String](classOf[Any], List(Tag[Int].tag, Tag[String].tag))
```

## WeakTag

`WeakTag[T]` is a low-level tag used by dependency injection frameworks to describe signatures of generic methods. When a `Tag[T]` is available, `WeakTag[T]` uses it. Application code should use `Tag`, which never contains unresolved type parameters. For tags that may contain unresolved type parameters, see `LTag.Weak` in [LTag and LTT](ltag.md).

## Running the Examples

All code from this page is available as runnable examples in the `izumi-reflect-examples` project.

### Basic Usage

The examples of the Usage section as one program: tags from context bounds, tags built from them, and comparisons.

```scala mdoc:embed:izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/tag/BasicUsage.scala:show-line-numbers
```

([source](https://github.com/zio/izumi-reflect/tree/develop/izumi-reflect/izumi-reflect-examples/src/main/scala-3/izumi/reflect/examples/tag/BasicUsage.scala)) Run it with `scala-cli run BasicUsage.scala` from that folder, or with `sbt "izumi-reflect-examples/runMain izumi.reflect.examples.tag.BasicUsage"` from the repository root.
