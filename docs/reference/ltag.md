---
id: ltag
title: "LTag and LTT"
sidebar_label: "LTag and LTT"
---

`LTag[T]` and `LTT[T]` create a [`LightTypeTag`](light-type-tag.md) for a type without going through a [`Tag`](tag.md). They live in the `izumi.reflect.macrortti` package. Unlike `Tag`, they never look for tags of type parameters in the implicit scope, so they are simpler and faster to compile, but they only describe types whose type parameters are all known at the call site.

You rarely need them in application code; they are lower-level building blocks for libraries and tests, prefer `Tag` when you write generic code.

## LTT

`LTT[T]` is a method that returns the `LightTypeTag` of `T` directly. It is the shortest way to get a tag for a concrete type, for example in tests:

```scala mdoc
import izumi.reflect.Tag
import izumi.reflect.macrortti._

LTT[Either[String, Int]]
LTT[Either[String, Int]] == Tag[Either[String, Int]].tag
```

`LTT` never fails on an abstract type parameter. It keeps the parameter as a named placeholder, the same way as [weak tags](#weak-tags) do, so the result does not describe the type the caller passes:

```scala mdoc
def lttOfList[T]: LightTypeTag = LTT[List[T]]

lttOfList[Int]
lttOfList[Int] == LTT[List[Int]]
```

Use `LTT` only for concrete types, and use `Tag` in generic code.

For type constructors, the `macrortti` package defines variants of `LTT` whose names spell out the kind, such as ``LTT[_]`` for `F[_]` and ``LTT[_,_]`` for `F[_, _]`. Their names contain brackets, so you call them with backticks:

```scala mdoc
val listConstructor = `LTT[_]`[List]
val eitherConstructor = `LTT[_,_]`[Either]
```

The full list is `LTT[_]`, `LTT[+_]`, `LTT[_,_]`, `LTT[_[_]]`, `LTT[_[+_]]`, `LTT[_[_[_]]]`, `LTT[_[+_[_]]]`, `LTT[_[_,_]]`, `LTT[_[+_,+_]]`, `LTT[_[_,_],_,_]`, `LTT[_[+_,+_],_,_]`, `LTT[_[_],_[_]]`, `LTT[_[_[_],_[_]]]`, plus two variants for bounded type parameters.

## LTag

`LTag[T]` is a type class that wraps a `LightTypeTag`, like `Tag` but without the class information and without combining tags of type parameters. Use it as an implicit parameter when you want a type tag for a fully known type:

```scala mdoc
def nameOf[T](implicit ltag: LTag[T]): String = ltag.tag.toString

nameOf[Map[String, Boolean]]
```

Because `LTag` doesn't combine tags, it fails to compile when the type mentions an abstract type parameter, even if a tag for that parameter is in scope. For example, `def listOf[T: Tag] = LTag[List[T]]` doesn't compile; use `Tag[List[T]]` for that.

For type constructors, the package defines `LTagK`, `LTagKK`, `LTagK3`, `LTagT`, `LTagTK`, `LTagTKK` and `LTagTK3`, which mirror the [`TagK` family](tag.md#tags-for-type-constructors):

```scala mdoc
LTagK[Option].tag
```

## Weak Tags

`LTag.Weak[T]` (also available as `LWeakTag[T]`) is a variant of `LTag` that accepts types with unresolved type parameters. Instead of failing, it keeps each unresolved type parameter as a named placeholder in the tag:

```scala mdoc
def weakListTag[T]: LightTypeTag = LTag.Weak[List[T]].tag

weakListTag[Int]
```

The result contains a placeholder for `T`, not `Int`: the macro ignores the type argument at the call site. A weak tag is only equal to another weak tag created from the same type parameter, so it is not suitable for comparing types across call sites. Dependency injection frameworks use weak tags to describe generic method signatures; application code should use `Tag`.

## When to Use Which

The tag creators differ in how they treat type parameters:

| Creator       | Result         | Abstract type parameters                        | Typical use                         |
| ------------- | -------------- | ----------------------------------------------- | ----------------------------------- |
| `Tag[T]`      | `Tag[T]`       | Resolved from `Tag`s in scope                   | Generic application and library code |
| `LTag[T]`     | `LTag[T]`      | Compile error                                   | Implicit tags for concrete types    |
| `LTT[T]`      | `LightTypeTag` | Kept as named placeholders, without an error    | Tests and one-off concrete tags     |
| `LTag.Weak[T]`| `LTag.Weak[T]` | Kept as named placeholders                      | Describing generic signatures       |
