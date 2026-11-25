/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect

import izumi.reflect.dottyreflection.Inspect
import izumi.reflect.macrortti.{LTag, LightTypeTag, LightTypeTagRef}

import scala.annotation.implicitNotFound

/**
  * The common parent of [[Tag]], [[HKTag]] and [[WeakTag]]: a runtime description of a type, made of a
  * [[izumi.reflect.macrortti.LightTypeTag]] and the JVM class closest to that type.
  *
  * Two tags are equal when their `tag` fields are equal; `closestClass` plays no part in `equals` and `hashCode`.
  * Tags are `java.io.Serializable`.
  *
  * {{{
  * trait Animal
  * trait Dog extends Animal
  *
  * val isChild: Boolean = Tag[Dog] <:< Tag[Animal] // true
  * val sameType: Boolean = Tag[List[Int]] =:= Tag[scala.collection.immutable.List[Int]] // true
  * }}}
  *
  * @see [[https://zio.dev/izumi-reflect/reference/tag/ Tag and the TagK Family]]
  */
trait AnyTag extends Serializable {
  /** The [[izumi.reflect.macrortti.LightTypeTag]] that describes the type; it does the comparisons and the rendering. */
  def tag: LightTypeTag

  /**
    * The JVM class closest to the type: the class itself for a class or trait, the class of the aliased type for a
    * type alias, and a common parent of the members (often `Object`) for an intersection type.
    *
    * A Scala type may have no JVM class of its own, so this class may not be enough to create instances of the type.
    * Reflect on it only when [[hasPreciseClass]] returns `true`.
    */
  def closestClass: Class[_]

  /**
    * Returns `true` when the short name of the type is the simple name of [[closestClass]], so that the class stands
    * for the type itself. Returns `false` for an intersection type such as `java.io.Serializable with CharSequence`,
    * whose closest class is `Object`.
    */
  final def hasPreciseClass: Boolean = {
    try tag.shortName == closestClass.getSimpleName
    catch {
      case i: InternalError if i.getMessage == "Malformed class name" => false
    }
  }

  /** Returns `true` when both tags describe the same type, comparing their `tag` fields with `LightTypeTag#=:=`. */
  final def =:=(that: AnyTag): Boolean = {
    tag =:= that.tag
  }

  /** Returns `true` when the type of this tag is a subtype of the type of `that`, by `LightTypeTag#<:<`. */
  final def <:<(that: AnyTag): Boolean = {
    tag <:< that.tag
  }

  override final def equals(that: Any): Boolean = that match {
    case that: AnyTag => this.tag == that.tag
    case _ => false
  }

  override final def hashCode(): Int = tag.hashCode()

  override def toString: String = s"AnyTag[$tag]"
}

/**
  * A type class that describes the type `T` at runtime. Ask for it with a context bound (`def f[T: Tag]`): the
  * compiler builds the tag at compile time and, for a type that mentions type parameters, combines the tags of those
  * parameters found in scope. It plays the role of `scala.reflect.api.TypeTags.TypeTag`, also for type constructors
  * (see `TagK` and its siblings), and it works on Scala 3 too.
  *
  * {{{
  * def keyOf[A: Tag]: String = Tag[A].tag.toString
  * def containerOf[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]
  *
  * val key: String = keyOf[Map[String, Int]] // "Map[=String,+Int]"
  * val same: Boolean = containerOf[Option, Int] =:= Tag[Option[Int]] // true
  * }}}
  *
  * When a type parameter has no tag in scope, compilation fails with a message that contains
  * `could not find implicit value for izumi.reflect.Tag[T]`. Add a context bound for that parameter.
  *
  * Limits, on Scala 2 and Scala 3:
  *  - Type parameters are not substituted inside the methods and values of a structural type: with `T = Int`,
  *    `Tag[{ def x: T }]` is not equal to `Tag[{ def x: Int }]`. They are substituted in type members:
  *    `Tag[Any { type Out = T }]` is equal to `Tag[Any { type Out = Int }]`.
  *  - `TagK` and its siblings take type constructors whose type parameters have no bounds: the compiler rejects
  *    `TagK[Abc]` for `class Abc[S <: String]`. On Scala 2, declare an [[HKTag]] alias with the same bound; on
  *    Scala 3, write `Tag[Abc]`.
  *
  * @see [[https://zio.dev/izumi-reflect/reference/tag/ Tag and the TagK Family]]
  * @see [[https://blog.7mind.io/lightweight-reflection.html Lightweight Scala Reflection and why Dotty needs TypeTags reimplemented]]
  * @see [[izumi.reflect.macrortti.LTag]] for the `LightTypeTag` of a type without abstract type parameters
  * @see [[izumi.reflect.macrortti.LTag.Weak]] for a `LightTypeTag` that keeps unresolved type parameters as placeholders
  */
@implicitNotFound(
  "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
)
trait Tag[T <: AnyKind] extends AnyTag {
  /** The [[izumi.reflect.macrortti.LightTypeTag]] of `T`. */
  def tag: LightTypeTag
  /** The JVM class closest to `T`; see `AnyTag#closestClass`. */
  def closestClass: Class[_]

  override final def toString: String = s"Tag[$tag]"
}

/** Summoners and constructors of [[Tag]], and the implicit macro that builds tags at compile time. */
object Tag {

  /**
    * `Tag.auto.T[F]` names the tag type for a type constructor `F` of any kind, for use as a context bound when no
    * `TagK` alias fits the kind. On Scala 3 it is an alias of `Tag[F]`. On Scala 2 it is a macro that expands to the
    * [[HKTag]] type for the kind of `F`; write it only as a context bound or as `implicitly[Tag.auto.T[F]]`, and not
    * for a proper type such as `Int`, where the Scala 2 macro fails during compilation.
    *
    * {{{
    * def tagOfAnyKind[K[_[_, _], _[_], _[_[_], _, _, _]]: Tag.auto.T]: Tag.auto.T[K] = implicitly[Tag.auto.T[K]]
    * }}}
    */
  object auto {
    /** The same type as `Tag[X]`. */
    type T[X <: AnyKind] = Tag[X]
  }

  /** Returns the implicit `Tag[T]` in scope: `Tag[Int]` is the same as `implicitly[Tag[Int]]`. */
  @inline def apply[T <: AnyKind: Tag]: Tag[T] = implicitly

  /**
    * Creates a `Tag[T]` from a class and a [[izumi.reflect.macrortti.LightTypeTag]], for example one read back with
    * `LightTypeTag.parse`. Nothing checks that `tag0` describes `T`.
    */
  def apply[T <: AnyKind](cls: Class[_], tag0: LightTypeTag): Tag[T] = {
    new Tag[T] {
      override val tag: LightTypeTag = tag0
      override val closestClass: Class[_] = cls
    }
  }

  /**
    * Creates the tag of the type formed by applying the type constructor in `tag` to `args`, in order. Nothing checks
    * that the result describes `R`.
    *
    * {{{
    * val listOfInt: Tag[List[Int]] = Tag.appliedTag[List[Int]](TagK[List], List(Tag[Int].tag))
    * val same: Boolean = listOfInt == Tag[List[Int]] // true
    * }}}
    */
  def appliedTag[R <: AnyKind](tag: Tag[_], args: List[LightTypeTag]): Tag[R] = {
    Tag(tag.closestClass, tag.tag.combine(args: _*))
  }

  /**
    * Like `appliedTag`, but takes one `Option` per type parameter: `Some` fills that parameter and `None` leaves it
    * open, so the result is a type constructor of the `None` positions.
    */
  def appliedTagNonPos[R <: AnyKind](tag: Tag[_], args: List[Option[LightTypeTag]]): Tag[R] = {
    Tag(tag.closestClass, tag.tag.combineNonPos(args: _*))
  }

  /** Like `appliedTagNonPos`, but takes the class and the `LightTypeTag` of the type constructor instead of a tag. */
  def appliedTagNonPosAux[R <: AnyKind](cls: Class[_], ctor: LightTypeTag, args: List[Option[LightTypeTag]]): Tag[R] = {
    Tag(cls, ctor.combineNonPos(args: _*))
  }

  /** Creates the tag of the union of the types in `union`, with `lubClass` as its closest class. */
  def unionTag[R <: AnyKind](lubClass: Class[_], union: List[LightTypeTag]): Tag[R] = {
    Tag(lubClass, LightTypeTag.unionType(union))
  }

  /**
    * Creates the tag of the intersection of the `intersection` types with a structural refinement, such as
    * `(A with B) { def close(): Unit }`. Only the refinement part of `structType` is used, so a weak tag of the whole
    * refined type and a weak tag of the bare refinement give the same result. Each entry of `additionalTypeMembers`
    * adds a type member (`type Name = Type`) and replaces a member of `structType` with the same name.
    *
    * {{{
    * import izumi.reflect.macrortti.LTag
    *
    * trait Reader
    * trait Writer
    * type Resource = (Reader with Writer) { def close(): Unit }
    *
    * val parts = List(LTag[Reader].tag, LTag[Writer].tag)
    * val resourceTag: Tag[Resource] = Tag.refinedTag[Resource](classOf[Any], parts, LTag.Weak[{ def close(): Unit }].tag, Map.empty)
    * val same: Boolean = resourceTag == Tag[Resource] // true
    * }}}
    */
  def refinedTag[R <: AnyKind](
    lubClass: Class[_],
    intersection: List[LightTypeTag],
    structType: LightTypeTag,
    additionalTypeMembers: Map[String, LightTypeTag]
  ): Tag[R] = {
    Tag(lubClass, LightTypeTag.refinedType(intersection, structType, additionalTypeMembers))
  }

  /** The implicit macro that builds a `Tag[T]` at compile time. You don't call it yourself; it shows in implicit hints and macro errors. */
  inline implicit final def tagFromTagMacro[T <: AnyKind]: Tag[T] = ${ TagMacro.createTagExpr[T] }
}

/**
  * A tag for a type constructor of any kind. On Scala 3 it is an alias of [[Tag]]. On Scala 2, `HKTag` takes a
  * structural type that spells out the kind: `TagK[K]` is `HKTag[{ type Arg[A] = K[A] }]`. Declare such an alias for a
  * kind that has no predefined one, or for type parameters with bounds, or use `Tag.auto.T`.
  *
  * {{{
  * // scala-2-only
  * trait Dep
  * type TagKDep[K[_ <: Dep]] = HKTag[{ type Arg[A <: Dep] = K[A] }]
  * type TagFGC[K[_[_, _], _[_], _[_[_], _, _, _]]] = HKTag[{ type Arg[A[_, _], B[_], C[_[_], _, _, _]] = K[A, B, C] }]
  * }}}
  *
  * @see [[https://zio.dev/izumi-reflect/reference/scala-2-vs-scala-3/ Scala 2 vs Scala 3]]
  */
type HKTag[T <: AnyKind] = Tag[T]

/** Constructors of [[HKTag]]. */
object HKTag {
  /**
    * Creates an `HKTag[T]` from a class and a [[izumi.reflect.macrortti.LightTypeTag]]; nothing checks that the tag
    * describes `T`. On Scala 3, this method accepts only a proper type `T`: use `Tag.apply` for a type constructor.
    */
  def apply[T](cls: Class[_], lightTypeTag: LightTypeTag): Tag[T] = Tag(cls, lightTypeTag)

  /** The same as `Tag.appliedTag`. */
  def appliedTag[R <: AnyKind](tag: Tag[_], args: List[LightTypeTag]): Tag[R] = Tag.appliedTag(tag, args)

  /**
    * Like `appliedTag`, but takes one `Option` per type parameter: `Some` fills that parameter and `None` leaves it
    * open, so the result is a type constructor of the `None` positions.
    */
  def appliedTagNonPos[R <: AnyKind](tag: Tag[_], args: List[Option[LightTypeTag]]): Tag[R] = Tag.appliedTagNonPos(tag, args)

  /**
    * Like `appliedTagNonPos`, but takes the class and the `LightTypeTag` of the type constructor instead of a tag. On
    * Scala 3, this method accepts only a proper type `R`: use `Tag.appliedTagNonPosAux` for a type constructor.
    */
  def appliedTagNonPosAux[R](cls: Class[_], ctor: LightTypeTag, args: List[Option[LightTypeTag]]): Tag[R] = Tag.appliedTagNonPosAux(cls, ctor, args)
}

/**
  * A tag for a type constructor with one type parameter, such as `Option` or `List`. Ask for it with a context bound
  * (`F[_]: TagK`) in code that is generic over a type constructor; the compiler then builds the tags of types made
  * from `F`. On Scala 2 it is an alias of `HKTag[{ type Arg[A] = K[A] }]`; on Scala 3, an alias of `Tag[K]`.
  *
  * {{{
  * import izumi.reflect._
  *
  * def containerTypesEqual[F[_]: TagK, K[_]: TagK]: Boolean = TagK[F].tag =:= TagK[K].tag
  *
  * val sameSet: Boolean = containerTypesEqual[Set, scala.collection.immutable.Set] // true
  * val arrayIsList: Boolean = containerTypesEqual[Array, List] // false
  * }}}
  *
  * @see [[https://zio.dev/izumi-reflect/reference/tag/ Tag and the TagK Family]]
  */
type TagK[K[_]] = Tag[K]
/** A tag for a type constructor with two type parameters, such as `Either` or `Map`. */
type TagKK[K[_, _]] = Tag[K]
/** A tag for a type constructor with three type parameters, such as `Function2`. */
type TagK3[K[_, _, _]] = Tag[K]

/** A tag for a type constructor whose one type parameter is itself a type constructor `F[_]`, such as a type class `Functor[F[_]]`. */
type TagT[K[_[_]]] = Tag[K]
/** A tag for a type constructor with type parameters `[F[_], A]`, such as a monad transformer `OptionT[F[_], A]`. */
type TagTK[K[_[_], _]] = Tag[K]
/** A tag for a type constructor with type parameters `[F[_], A, B]`, such as `EitherT[F[_], A, B]`. */
type TagTKK[K[_[_], _, _]] = Tag[K]
/** A tag for a type constructor with type parameters `[F[_], A, B, C]`. */
type TagTK3[K[_[_], _, _, _]] = Tag[K]

/** Summoner of `TagK`. */
object TagK {
  /** Returns the implicit `TagK[K]` in scope, as in `TagK[Option]`. */
  @inline def apply[K[_]: TagK]: TagK[K] = implicitly
}

/** Summoner of `TagKK`. */
object TagKK {
  /** Returns the implicit `TagKK[K]` in scope, as in `TagKK[Either]`. */
  @inline def apply[K[_, _]: TagKK]: TagKK[K] = implicitly
}

/** Summoner of `TagK3`. */
object TagK3 {
  /** Returns the implicit `TagK3[K]` in scope, as in `TagK3[Function2]`. */
  @inline def apply[K[_, _, _]: TagK3]: TagK3[K] = implicitly
}

/** Summoner of `TagT`. */
object TagT {
  /** Returns the implicit `TagT[K]` in scope. */
  @inline def apply[K[_[_]]: TagT]: TagT[K] = implicitly
}

/** Summoner of `TagTK`. */
object TagTK {
  /** Returns the implicit `TagTK[K]` in scope. */
  @inline def apply[K[_[_], _]: TagTK]: TagTK[K] = implicitly
}

/** Summoner of `TagTKK`. */
object TagTKK {
  /** Returns the implicit `TagTKK[K]` in scope. */
  @inline def apply[K[_[_], _, _]: TagTKK]: TagTKK[K] = implicitly
}

/** Summoner of `TagTK3`. */
object TagTK3 {
  /** Returns the implicit `TagTK3[K]` in scope. */
  @inline def apply[K[_[_], _, _, _]: TagTK3]: TagTK3[K] = implicitly
}

/** A tag for a type constructor with 4 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK4[K[_, _, _, _]] = Tag[K]
/** A tag for a type constructor with 5 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK5[K[_, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 6 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK6[K[_, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 7 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK7[K[_, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 8 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK8[K[_, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 9 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK9[K[_, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 10 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK10[K[_, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 11 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK11[K[_, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 12 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK12[K[_, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 13 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK13[K[_, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 14 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK14[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 15 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK15[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 16 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK16[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 17 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK17[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 18 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK18[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 19 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK19[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 20 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK20[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 21 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK21[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
/** A tag for a type constructor with 22 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
type TagK22[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]

/**
  * A tag that may contain unresolved type parameters. When a `Tag[T]` is in scope, `WeakTag[T]` holds the same type;
  * otherwise it falls back to `LTag.Weak[T]`, which keeps each type parameter that has no tag as a named placeholder,
  * with `Object` as the closest class. On Scala 3 there is no such fallback for a type parameter without a tag:
  * `WeakTag[T]` and `WeakTag[List[T]]` fail to compile with the missing `Tag` error.
  *
  * Dependency injection frameworks use it to describe the signatures of generic methods; other code should use [[Tag]].
  *
  * {{{
  * // scala-2-only
  * def weakListTag[T]: WeakTag[List[T]] = WeakTag[List[T]]
  *
  * val placeholder: String = weakListTag[Int].tag.toString // "List[+T]"
  * }}}
  */
trait WeakTag[T <: AnyKind] extends AnyTag {
  /** The [[izumi.reflect.macrortti.LightTypeTag]] of `T`, with placeholders for unresolved type parameters. */
  def tag: LightTypeTag
  /** The JVM class closest to `T`; `Object` when the tag comes from `LTag.Weak`. */
  def closestClass: Class[_]

  override final def toString: String = s"WeakTag[$tag]"
}

/** Summoners and constructors of [[WeakTag]]. */
object WeakTag extends WeakTagInstances1 {
  /** Returns the implicit `WeakTag[T]` in scope. */
  @inline def apply[T <: AnyKind: WeakTag]: WeakTag[T] = implicitly

  /** Creates a `WeakTag[T]` from a class and a [[izumi.reflect.macrortti.LightTypeTag]]. */
  def apply[T <: AnyKind](cls: Class[_], l: LightTypeTag): WeakTag[T] = {
    new WeakTag[T] {
      override val tag: LightTypeTag = l
      override val closestClass: Class[_] = cls
    }
  }

  /** Builds a `WeakTag[T]` from the `Tag[T]` in scope, with the same `tag` and `closestClass`. */
  implicit def weakTagFromTag[T <: AnyKind](implicit t: Tag[T]): WeakTag[T] = WeakTag(t.closestClass, t.tag)
}
/** The lower-priority fallback of [[WeakTag]] to `LTag.Weak`. */
trait WeakTagInstances1 {
  /** Builds a `WeakTag[T]` from `LTag.Weak[T]`, with `Object` as the closest class. */
  implicit final def weakTagFromWeakTypeTag[T <: AnyKind](implicit l: LTag.Weak[T]): WeakTag[T] = WeakTag(classOf[Any], l.tag)
}
