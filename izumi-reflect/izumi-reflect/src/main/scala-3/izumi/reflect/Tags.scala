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

trait AnyTag extends Serializable {
  def tag: LightTypeTag

  /**
    * Closest class found for the type or for a LUB of all intersection
    * members in case of an intersection type.
    *
    * A Scala type may not have an associated JVM class, as such
    * this class may not be sufficient to create instances of `T`
    *
    * Only if `tag.hasPreciseClass` returns true
    * it may be safe to reflect on `closestClass`
    */
  def closestClass: Class[_]

  final def hasPreciseClass: Boolean = {
    try tag.shortName == closestClass.getSimpleName
    catch {
      case i: InternalError if i.getMessage == "Malformed class name" => false
    }
  }

  override final def equals(that: Any): Boolean = that match {
    case that: AnyTag => this.tag == that.tag
    case _ => false
  }

  override final def hashCode(): Int = tag.hashCode()
}

/**
  * Like [[scala.reflect.api.TypeTags.TypeTag]], but supports higher-kinded type tags via `TagK` type class.
  *
  * In context of DI this lets you define modules parameterized by higher-kinded type parameters.
  * This is especially helpful for applying [[https://www.beyondthelines.net/programming/introduction-to-tagless-final/ `tagless final` style]]
  *
  * Example:
  * {{{
  * import distage.ModuleDef
  *
  * class MyModule[F[_]: Monad: TagK] extends ModuleDef {
  *   make[MyService[F]]
  *   make[F[Int]].named("lucky-number").from(Monad[F].pure(7))
  * }
  * }}}
  *
  * Without a `TagK` constraint above, this example would fail with `no TypeTag available for MyService[F]` error
  *
  * Currently some limitations apply as to when a `Tag` will be correctly constructed:
  *   * Type Parameters do not yet resolve in structural refinement methods, e.g. T in {{{ Tag[{ def x: T}] }}}
  *     They do resolve in refinement type members however, e.g. {{{ Tag[ Any { type Out = T } ] }}}
  *   * TagK* does not resolve for constructors with bounded parameters, e.g. S in {{{ class Abc[S <: String]; TagK[Abc] }}}
  *     (You can still have a bound in partial application: e.g. {{{ class Abc[S <: String, A]; TagK[Abc["hi", _]] }}}
  *   * Further details at [[https://github.com/7mind/izumi/issues/374]]
  *
  * @see "Lightweight Scala Reflection and why Dotty needs TypeTags reimplemented" https://blog.7mind.io/lightweight-reflection.html
  *
  * @see [[izumi.reflect.macrortti.LTag]] - summoner for [[izumi.reflect.macrortti.LightTypeTag]] that does not resolve type parameters
  * @see [[izumi.reflect.macrortti.LTag.Weak]] - summoner for [[izumi.reflect.macrortti.LightTypeTag]] that does not resolve type parameters and allows unresolved ("weak") type parameters to be part of a tag
  */
@implicitNotFound(
  "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."
)
trait Tag[T <: AnyKind] extends AnyTag {
  def tag: LightTypeTag
  def closestClass: Class[_]

  override final def toString: String = s"Tag[$tag]"
}

object Tag {

  /**
    * Use `Tag.auto.T[TYPE_PARAM]` syntax to summon a `Tag` for a type parameter of any kind:
    *
    * NOTE: On Scala 3+ it's the same as `Tag[T]`
    */
  object auto {
    type T[X <: AnyKind] = Tag[X]
  }

  @inline def apply[T <: AnyKind: Tag]: Tag[T] = implicitly

  def apply[T <: AnyKind](cls: Class[_], tag0: LightTypeTag): Tag[T] = {
    new Tag[T] {
      override val tag: LightTypeTag = tag0
      override val closestClass: Class[_] = cls
    }
  }

  /**
    * Create a Tag of a type formed by applying the type in `tag` to `args`.
    *
    * Example:
    * {{{
    *   implicit def tagFromTagTAKA[T[_, _[_], _]: TagK3, K[_]: TagK, A0: Tag, A1: Tag]: Tag[T[A0, K, A1]] =
    *     Tag.appliedTag(TagK3[T].tag, List(Tag[A0].tag, TagK[K].tag, Tag[A1].tag))
    * }}}
    */
  def appliedTag[R <: AnyKind](tag: Tag[_], args: List[LightTypeTag]): Tag[R] = {
    Tag(tag.closestClass, tag.tag.combine(args: _*))
  }

  def appliedTagNonPos[R <: AnyKind](tag: Tag[_], args: List[Option[LightTypeTag]]): Tag[R] = {
    Tag(tag.closestClass, tag.tag.combineNonPos(args: _*))
  }

  def appliedTagNonPosAux[R <: AnyKind](cls: Class[_], ctor: LightTypeTag, args: List[Option[LightTypeTag]]): Tag[R] = {
    Tag(cls, ctor.combineNonPos(args: _*))
  }

  def unionTag[R <: AnyKind](lubClass: Class[_], union: List[LightTypeTag]): Tag[R] = {
    Tag(lubClass, LightTypeTag.unionType(union))
  }

  /**
    * Create a Tag of a type formed from an `intersection` of types (A with B) with a structural refinement taken from `structType`
    *
    * `structType` is assumed to be contain the structural refinements of the entire type, e.g.
    * {{{
    *   Tag[A with B {def abc: Unit}] == refinedTag(classOf[Any], List(LTag[A].tag, LTag[B].tag), LTag.Weak[{ def abc: Unit }].tag, Map.empty)
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

  inline implicit final def tagFromTagMacro[T <: AnyKind]: Tag[T] = ${ TagMacro.createTagExpr[T] }
}

/**
  * Internal unsafe API representing a poly-kinded, higher-kinded type tag.
  *
  * NOTE: On Scala 3+ it's the same as `Tag[T]`
  */
type HKTag[T <: AnyKind] = Tag[T]

object HKTag {
  def apply[T](cls: Class[_], lightTypeTag: LightTypeTag): Tag[T] = Tag(cls, lightTypeTag)

  def appliedTag[R <: AnyKind](tag: Tag[_], args: List[LightTypeTag]): Tag[R] = Tag.appliedTag(tag, args)

  def appliedTagNonPos[R <: AnyKind](tag: Tag[_], args: List[Option[LightTypeTag]]): Tag[R] = Tag.appliedTagNonPos(tag, args)

  def appliedTagNonPosAux[R](cls: Class[_], ctor: LightTypeTag, args: List[Option[LightTypeTag]]): Tag[R] = Tag.appliedTagNonPosAux(cls, ctor, args)
}

/**
  * `TagK` is like a [[scala.reflect.api.TypeTags.TypeTag]] but for higher-kinded types.
  *
  * NOTE: On Scala 3+ it's the same as `Tag[T]`
  */
type TagK[K[_]] = Tag[K]
type TagKK[K[_, _]] = Tag[K]
type TagK3[K[_, _, _]] = Tag[K]

type TagT[K[_[_]]] = Tag[K]
type TagTK[K[_[_], _]] = Tag[K]
type TagTKK[K[_[_], _, _]] = Tag[K]
type TagTK3[K[_[_], _, _, _]] = Tag[K]

object TagK {
  /**
    * Construct a type tag for a higher-kinded type `K[_]`
    *
    * Example:
    * {{{
    *     TagK[Option]
    * }}}
    */
  @inline def apply[K[_]: TagK]: TagK[K] = implicitly
}

object TagKK {
  @inline def apply[K[_, _]: TagKK]: TagKK[K] = implicitly
}

object TagK3 {
  @inline def apply[K[_, _, _]: TagK3]: TagK3[K] = implicitly
}

object TagT {
  @inline def apply[K[_[_]]: TagT]: TagT[K] = implicitly
}

object TagTK {
  @inline def apply[K[_[_], _]: TagTK]: TagTK[K] = implicitly
}

object TagTKK {
  @inline def apply[K[_[_], _, _]: TagTKK]: TagTKK[K] = implicitly
}

object TagTK3 {
  @inline def apply[K[_[_], _, _, _]: TagTK3]: TagTK3[K] = implicitly
}

type TagK4[K[_, _, _, _]] = Tag[K]
type TagK5[K[_, _, _, _, _]] = Tag[K]
type TagK6[K[_, _, _, _, _, _]] = Tag[K]
type TagK7[K[_, _, _, _, _, _, _]] = Tag[K]
type TagK8[K[_, _, _, _, _, _, _, _]] = Tag[K]
type TagK9[K[_, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK10[K[_, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK11[K[_, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK12[K[_, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK13[K[_, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK14[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK15[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK16[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK17[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK18[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK19[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK20[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK21[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]
type TagK22[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = Tag[K]

// Workaround needed specifically to support generic methods in factories, see `GenericAssistedFactory` and related tests
//
// We need to construct a SafeType signature for a generic method, but generic parameters have no type tags
// So we resort to weak type parameters and pointer equality
trait WeakTag[T <: AnyKind] extends AnyTag {
  def tag: LightTypeTag
  def closestClass: Class[_]

  override final def toString: String = s"WeakTag[$tag]"
}

object WeakTag extends WeakTagInstances1 {
  @inline def apply[T <: AnyKind: WeakTag]: WeakTag[T] = implicitly

  def apply[T <: AnyKind](cls: Class[_], l: LightTypeTag): WeakTag[T] = {
    new WeakTag[T] {
      override val tag: LightTypeTag = l
      override val closestClass: Class[_] = cls
    }
  }

  implicit def weakTagFromTag[T <: AnyKind](implicit t: Tag[T]): WeakTag[T] = WeakTag(t.closestClass, t.tag)
}
trait WeakTagInstances1 {
  implicit final def weakTagFromWeakTypeTag[T <: AnyKind](implicit l: LTag.Weak[T]): WeakTag[T] = WeakTag(classOf[Any], l.tag)
}
