---
id: zio-environment
title: "Use izumi-reflect Tags with the ZIO Environment"
description: "Write generic ZIO services and layers by adding the Tag and TagK context bounds that the ZIO environment needs."
keywords:
  - "ZIO Environment"
  - "ZLayer"
  - "Tag"
  - "TagK"
  - "Dependency Injection"
---

In this guide, we write generic ZIO code that reads services from the environment: an accessor for any service type, a layer that provides a service for any type parameters, and a service generic in an effect type. Along the way we see why ZIO asks for a `Tag` in each of these places and how to satisfy it. ZIO 2 uses `izumi-reflect` to identify services, so everything here relies on the tags described in the rest of this documentation.

## The Problem

The environment of a ZIO effect, `R` in `ZIO[R, E, A]`, is a map from service types to service values. To find a service at runtime, ZIO needs a value that describes its type, and in ZIO 2 that value is an `izumi-reflect` tag. For concrete types, the compiler creates the tag automatically, so most ZIO code never mentions it. In generic code, it can't:

```scala
// does not compile
def serviceOf[A]: URIO[A, A] = ZIO.service[A]
```

The compiler reports that `A` has no tag:

```text
A is a type parameter without an implicit Tag!
  could not find implicit value for izumi.reflect.Tag[A]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in A? e.g. def x[T: Tag, F[_]: TagK] = ...
```

The fix is always to pass the tag in from the call site, where the type is known.

## Prerequisites

ZIO 2 depends on `izumi-reflect`, so a ZIO project needs no extra dependency:

```scala
libraryDependencies += "dev.zio" %% "zio" % "<zio version>"
```

The `zio` package re-exports the types you need: `zio.Tag`, a subtype of `izumi.reflect.Tag`, and aliases for `izumi.reflect.TagK`, `TagKK` and the rest of the family, `LightTypeTag`, and `EnvironmentTag`, an alias of `izumi.reflect.Tag`. All examples use one import:

```scala mdoc:silent
import zio._
```

To run the effects on this page, we use a small helper that runs a ZIO effect and returns its result:

```scala mdoc:silent
def run[A](effect: ZIO[Any, Nothing, A]): A =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
  }
```

## Accessing Any Service

To write an accessor that works for any service type, add a `Tag` context bound to its type parameter. The caller's concrete type then carries its tag into the method, and `ZIO.service` uses it to find the service:

```scala mdoc:silent
def serviceOf[A: Tag]: URIO[A, A] = ZIO.service[A]

final case class Config(url: String)
```

To provide a `Config` and read it through the generic accessor:

```scala mdoc
run(serviceOf[Config].provide(ZLayer.succeed(Config("jdbc:h2:mem"))))
```

## Writing Generic Layers

A layer that is generic in its type parameters needs a tag for every type parameter that appears in the service it provides. With them, ZIO keeps services that differ only in their type arguments apart, because their tags differ. For example, a generic in-memory cache:

```scala mdoc:silent
final case class Cache[K, V](entries: Map[K, V]) {
  def get(key: K): Option[V] = entries.get(key)
}

def cacheLayer[K: Tag, V: Tag](entries: (K, V)*): ULayer[Cache[K, V]] =
  ZLayer.succeed(Cache(entries.toMap))
```

`izumi-reflect` builds `Tag[Cache[K, V]]` from the tags of `K` and `V`. An effect can then require two caches that differ only in their type arguments:

```scala mdoc:silent
val lookups: URIO[Cache[String, Int] with Cache[Int, String], (Option[Int], Option[String])] =
  for {
    byName <- ZIO.serviceWith[Cache[String, Int]](_.get("answer"))
    byId   <- ZIO.serviceWith[Cache[Int, String]](_.get(42))
  } yield (byName, byId)
```

To provide both caches and run the effect:

```scala mdoc
run(lookups.provide(cacheLayer("answer" -> 42), cacheLayer(42 -> "answer")))
```

A lookup keyed by the class alone could not keep the two `Cache` services apart, because both have the class `Cache`.

## Writing Services Generic in an Effect Type

In tagless-final style, a service is generic in an effect type `F[_]`. To put such a service in the ZIO environment, the code that builds or reads it needs a `TagK[F]`, which describes the type constructor `F`. `izumi-reflect` combines it with the rest of the type to build `Tag[Repository[F]]`:

```scala mdoc:silent
trait Repository[F[_]] {
  def find(id: Int): F[String]
}

def repositoryLayer[F[_]: TagK](repo: Repository[F]): ULayer[Repository[F]] =
  ZLayer.succeed(repo)

def findWith[F[_]: TagK](id: Int): URIO[Repository[F], F[String]] =
  ZIO.serviceWith[Repository[F]](_.find(id))
```

An `Option`-based repository implements it for one effect type:

```scala mdoc:silent
val optionRepo: Repository[Option] = new Repository[Option] {
  def find(id: Int): Option[String] = if (id == 1) Some("Alice") else None
}
```

To provide the repository and call it through the generic accessor:

```scala mdoc
run(findWith[Option](1).provide(repositoryLayer(optionRepo)))
```

Without the `TagK` context bound on `findWith`, compilation fails with a message asking for a `Tag`, `TagK` or `TagKK` for `F`.

## Looking Up Services by Supertype

ZIO does not need an exact match to find a service. It looks for a service whose type is the requested type or a subtype of it, using `LightTypeTag#<:<`, and caches the result per requested tag. So an effect can ask for an interface while the environment holds an implementation registered under its own type:

```scala mdoc:silent
trait Clock2 { def now: Long }
final case class FixedClock(now: Long) extends Clock2
```

To store a `FixedClock` and read it as a `Clock2`:

```scala mdoc
val environment: ZEnvironment[FixedClock] = ZEnvironment(FixedClock(1700000000L))

environment.get[Clock2].now
```

When several services match the requested type, ZIO returns the one added most recently. To avoid surprises, register each service under the type its users ask for, as `ZLayer.succeed[Clock2](FixedClock(...))` does.

## Common Mistakes

Most problems with tags in ZIO code have one of three causes:

- **A missing context bound.** The compile error names the type that has no tag. Add `A: Tag` for a proper type, or `F[_]: TagK` for a type constructor, to the method that has the type parameter, and to every generic method that calls it.
- **A tag for the wrong type.** When `A` is inferred, the tag describes the inferred type, which may be more specific than you meant, such as `FixedClock` instead of `Clock2`. Write the type argument explicitly in generic calls like `ZLayer.succeed[Clock2](...)`.
- **Abstract type members.** A service type that refers to an abstract type member, such as `x.Out`, needs a tag for that member in scope, like any other type parameter.

To see why a lookup fails at runtime, print the tags involved with `Tag[A].tag` and compare them, or follow [Debug a Wrong Tag or Subtype Check](debug-a-wrong-tag.md).

## Going Further

The ZIO environment is a type-indexed map like the one in [Build a Type-Safe Heterogeneous Map](heterogeneous-map.md). For more about the tags themselves:

- [Tag and the TagK Family](../reference/tag.md) lists all tag types and their kinds.
- [LightTypeTag](../reference/light-type-tag.md#subtyping) explains how subtype checks work and where they are less precise than the compiler.
