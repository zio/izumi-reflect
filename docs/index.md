---
id: index
title: "Introduction to izumi-reflect"
sidebar_label: "izumi-reflect"
---

@PROJECT_BADGES@

`izumi-reflect` is a fast, portable, zero-dependency compile-time and runtime type reflection library for Scala 2.12, Scala 2.13, and Scala 3 across JVM, Scala.js, Scala Native, and GraalVM Native Image.

It models the Scala type system as an immutable, self-contained algebraic data type ([`LightTypeTag`](https://javadoc.io/doc/dev.zio/izumi-reflect_2.13/latest/izumi/reflect/macrortti/LightTypeTag.html)) together with a lightweight subtype checking simulator ([`LightTypeTagInheritance`](https://javadoc.io/doc/dev.zio/izumi-reflect_2.13/latest/izumi/reflect/macrortti/LightTypeTagInheritance.html)) and higher-kinded type combination engine ([`RuntimeAPI`](https://javadoc.io/doc/dev.zio/izumi-reflect_2.13/latest/izumi/reflect/macrortti/RuntimeAPI$.html)).

## Core Typeclasses and Entry Points

| API | Package | Purpose |
| :--- | :--- | :--- |
| `Tag[T]` | `izumi.reflect` | Summons a `LightTypeTag` and closest runtime `Class[_]` for a proper type `T` (or any kind `T <: AnyKind` on Scala 3), resolving abstract type parameters from implicit `Tag` / `TagK` / `HKTag` instances in scope. |
| `TagK[F[_]]`, `TagKK[F[_, _]]`, `TagK3[F[_, _, _]]` | `izumi.reflect` | Summons a `Tag` for higher-kinded type constructors of common kinds (`* -> *`, `(*, *) -> *`, `(*, *, *) -> *`). |
| `TagT[F[_[_]]]`, `TagTK[F[_[_], _]]` | `izumi.reflect` | Summons a `Tag` for higher-order type constructors (such as monad transformers `OptionT[F, A]`). |
| `Tag.auto.T[K]` | `izumi.reflect` | Whitebox macro (Scala 2) / type alias (Scala 3) that synthesizes a poly-kinded `HKTag` / `Tag` requirement for an arbitrary kind signature. |
| `LTag[T]` / `LTT[T]` | `izumi.reflect.macrortti` | Directly inspects a concrete ("strong") type `T` into a `LightTypeTag` at compile time without searching for implicit tags for abstract type parameters. |
| `LTag.Weak[T]` / `WeakTag[T]` | `izumi.reflect.macrortti` / `izumi.reflect` | Inspects types that may still contain unresolved ("weak") type parameters, preserving them as nominal references for generic factory signatures. |

## Summoning and Combining Tags

For concrete types, `LTT[T]` or `Tag[T]` materializes a `LightTypeTag` directly at compile time:

```scala
import izumi.reflect.{Tag, TagK}
import izumi.reflect.macrortti.LTT

val intListTag = Tag[List[Int]].tag
val seqAnyValTag = LTT[Seq[AnyVal]]

assert(intListTag <:< seqAnyValTag)
assert(!(seqAnyValTag <:< intListTag))
```

For polymorphic code parameterized by abstract types or higher-kinded type constructors (`F[_]`), `TagMacro` decomposes the type expression at compile time, summons the constituent `Tag` / `TagK` implicits, and combines their `LightTypeTag`s at runtime via `LightTypeTag#combine`:

```scala
import izumi.reflect.{Tag, TagK}

trait Repository[F[_], Entity]

def describeRepository[F[_]: TagK, A: Tag]: Tag[Repository[F, A]] = {
  // TagMacro synthesizes:
  // Tag.appliedTag(TagK[Repository], List(TagK[F].tag, Tag[A].tag))
  Tag[Repository[F, A]]
}

val repoTag = describeRepository[Option, String]
assert(repoTag.tag =:= Tag[Repository[Option, String]].tag)
```

## Architecture & Internals

Unlike `scala-reflect`'s `TypeTag`—which embeds serialized Scala 2 compiler universe trees and requires global runtime synchronization—`izumi-reflect` inspects types ahead-of-time inside macros and serializes only the minimal structural AST and inheritance databases required for runtime equality (`=:=`), subtype comparison (`<:<`), and type constructor application (`combine`).

See [**Architecture, Internals & Design Trade-offs**](internals.md) for an in-depth guide covering:

1. **Historical Motivation**: Why `scala.reflect.api.TypeTags.TypeTag` was replaced and how `izumi-reflect` evolved for Scala 2 and Scala 3.
2. **The Three-Part `LightTypeTag` Model**: How `ref: LightTypeTagRef`, `basesdb: Map[AbstractReference, Set[AbstractReference]]`, and `idb: Map[NameReference, Set[NameReference]]` work together.
3. **Alpha-Equivalence & De Bruijn Lambda Normalization**: How `LambdaParamName` and `Lambda.normalizedOutput` guarantee deterministic equality and hashing for type lambdas.
4. **Compile-Time Inspection Pipeline**: How Scala 2 (`LightTypeTagImpl`) and Scala 3 (`Inspector`, `FullDbInspector`, `InheritanceDbInspector`) extract type graphs and terminate F-bounded cycles.
5. **Shaded `BooPickle` Binary Serialization & Lazy Deserialization**: How `ParsedLightTypeTag230Plus` achieves `O(1)` string-literal equality without deserializing the tag graph.
6. **Runtime Type Application & `TagMacro` Assembly**: How `RuntimeAPI.Rewriter` substitutes higher-kinded type parameters and why unapplied base lambdas are retained in `basesdb`.
7. **Subtype Checking Engine (`LightTypeTagInheritance`)**: How variance, wildcards, intersections, unions, refinements, and higher-kinded parents are compared.
8. **Deliberate Design Decisions, Sacrifices & Limitations**: Which soundness properties of the full Scala typechecker were traded away and why.
