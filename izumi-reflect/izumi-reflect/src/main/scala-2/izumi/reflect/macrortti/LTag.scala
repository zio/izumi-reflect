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

package izumi.reflect.macrortti

import scala.language.experimental.macros

/**
  * A type class that wraps the [[LightTypeTag]] of `T`, built at compile time. Unlike [[izumi.reflect.Tag]], it holds no
  * class and does not combine tags of type parameters: it fails to compile for a type that mentions an abstract type
  * parameter, even when a `Tag` for that parameter is in scope. Use it for types known at the call site.
  *
  * {{{
  * import izumi.reflect.macrortti._
  *
  * def nameOf[T](implicit ltag: LTag[T]): String = ltag.tag.toString
  *
  * val name: String = nameOf[Map[String, Boolean]] // "Map[=String,+Boolean]"
  * }}}
  *
  * @see [[https://zio.dev/izumi-reflect/reference/ltag/ LTag and LTT]]
  */
final case class LTag[T](tag: LightTypeTag)

/**
  * Summoners of [[LTag]] and of its variants `LTag.Weak`, `LTag.StrongHK` and `LTag.WeakHK`. Each variant wraps one
  * `LightTypeTag`; they differ in how the macro treats type parameters and kinds.
  */
object LTag {
  /** Returns the implicit `LTag[T]` in scope. */
  def apply[T: LTag]: LTag[T] = implicitly

  /** The implicit macro that builds an `LTag[T]` at compile time. */
  implicit def materialize[T]: LTag[T] = macro LightTypeTagMacro.makeStrongTag[T]

  /**
    * An `LTag` that accepts types with unresolved type parameters: each type parameter without a known type stays a
    * named placeholder, and the type argument at the call site is ignored. Dependency injection frameworks use it to
    * describe generic signatures; other code should use [[izumi.reflect.Tag]]. Also available as `LWeakTag[T]`.
    *
    * {{{
    * import izumi.reflect.macrortti._
    *
    * def weakListTag[T]: LightTypeTag = LTag.Weak[List[T]].tag
    *
    * val same: Boolean = weakListTag[Int] == LTT[List[Int]] // false: the tag holds a placeholder for T
    * }}}
    *
    * @see [[https://zio.dev/izumi-reflect/reference/ltag/ LTag and LTT]]
    */
  final case class Weak[T](tag: LightTypeTag)
  /** Summoner of [[LTag.Weak]]. */
  object Weak {
    /** Returns the implicit `LTag.Weak[T]` in scope. */
    def apply[T: LTag.Weak]: LTag.Weak[T] = implicitly

    /** The implicit macro that builds an `LTag.Weak[T]` at compile time. */
    implicit def materialize[T]: LTag.Weak[T] = macro LightTypeTagMacro.makeWeakTag[T]
  }

  /**
    * The `LTag` of a type constructor, with the kind spelled out as in `izumi.reflect.HKTag`: the type behind `LTagK`
    * and its siblings. On Scala 3 it is an alias of `LTag`.
    */
  final case class StrongHK[T](tag: LightTypeTag)
  /** The implicit macro behind [[LTag.StrongHK]]. */
  object StrongHK {
    /** Builds an `LTag.StrongHK[T]` at compile time. */
    implicit def materialize[T]: LTag.StrongHK[T] = macro LightTypeTagMacro.makeStrongHKTag[T]
  }

  /**
    * The variant of `StrongHK` that keeps unresolved type parameters as placeholders, like `LTag.Weak`. On Scala 3 it is
    * an alias of `LTag.Weak`.
    */
  final case class WeakHK[T](tag: LightTypeTag)
  /** The implicit macro behind [[LTag.WeakHK]]. */
  object WeakHK {
    /** Builds an `LTag.WeakHK[T]` at compile time. */
    implicit def materialize[T]: LTag.WeakHK[T] = macro LightTypeTagMacro.makeWeakHKTag[T]
  }
}
