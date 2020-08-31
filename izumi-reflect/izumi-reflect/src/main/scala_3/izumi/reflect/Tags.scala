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
import izumi.reflect.macrortti.LightTypeTag

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

trait AnyTag {
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
    try tag.shortName == closestClass.getSimpleName catch {
      case i: InternalError if i.getMessage == "Malformed class name" => false
    }
  }
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
  *   * Type Parameters do not yet resolve inside structural refinements, e.g. T in {{{ Tag[{ def x: T}] }}}
  *   * TagK* does not resolve for constructors with bounded parameters, e.g. S in {{{ class Abc[S <: String]; TagK[Abc] }}}
  *     (You can still have a bound in partial application: e.g. {{{ class Abc[S <: String, A]; TagK[Abc["hi", ?]] }}}
  *   * Further details at [[https://github.com/7mind/izumi/issues/374]]
  *
  * @see "Lightweight Scala Reflection and why Dotty needs TypeTags reimplemented" https://blog.7mind.io/lightweight-reflection.html
  */
@implicitNotFound("could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ...")
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
    **/
  object auto { type T[T <: AnyKind] = Tag[T] }

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
    **/
  def appliedTag[R <: AnyKind](tag: Tag[_], args: List[LightTypeTag]): Tag[R] = {
    Tag(tag.closestClass, tag.tag.combine(args: _*))
  }

  /**
    * Create a Tag of a type formed from an `intersection` of types (A with B) with a structural refinement taken from `structType`
    *
    * `structType` is assumed to be a weak type of the entire type, e.g.
    * {{{
    *   Tag[A with B {def abc: Unit}] == refinedTag(classOf[Any], List(LTag[A].tag, LTag[B].tag), LTag.Weak[A with B { def abc: Unit }].tag, Map.empty)
    * }}}
    **/
  def refinedTag[R <: AnyKind](lubClass: Class[_], intersection: List[LightTypeTag], structType: LightTypeTag, additionalTypeMembers: Map[String, LightTypeTag]): Tag[R] = {
    Tag(lubClass, LightTypeTag.refinedType(intersection, structType, additionalTypeMembers))
  }

  inline implicit final def tagFromTagMacro[T]: Tag[T] = Tag(classOf[Any], Inspect.inspect[T])
}

/**
  * Internal unsafe API representing a poly-kinded, higher-kinded type tag.
  *
  * NOTE: On Scala 3+ it's the same as `Tag[T]`
  */
type HKTag[T <: AnyKind] = Tag[T]

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
    **/
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

// TODO
//  type TagKUBound[U, K[_ <: U]] = HKTag[{ type Arg[A <: U] = K[A] }]
//  object TagKUBound {
//    def apply[U, K[_ <: U]](implicit ev: TagKUBound[U, K]): TagKUBound[U, K] = implicitly
//  }

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
  def apply[T: WeakTag]: WeakTag[T] = implicitly

  def apply[T](cls: Class[_], l: LightTypeTag): WeakTag[T] = {
    new WeakTag[T] {
      override val tag: LightTypeTag = l
      override val closestClass: Class[_] = cls
    }
  }

  implicit def weakTagFromTag[T: Tag]: WeakTag[T] = WeakTag(Tag[T].closestClass, Tag[T].tag)
}
trait WeakTagInstances1 {
//    inline implicit final def weakTagFromWeakTypeTag[T](implicit l: LTag.Weak[T]): WeakTag[T] = WeakTag(classOf[Any], l.tag)
  inline implicit final def weakTagFromWeakTypeTag[T]: WeakTag[T] = scala.compiletime.error("not implemented")
}

