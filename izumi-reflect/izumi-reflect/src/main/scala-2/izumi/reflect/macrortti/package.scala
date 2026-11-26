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

import scala.language.experimental.macros

package object macrortti {
  /** Another name for `LTag.Weak[T]`. */
  type LWeakTag[T] = LTag.Weak[T]
  /** Summoner of `LWeakTag`. */
  object LWeakTag {
    /** Returns the implicit `LTag.Weak[T]` in scope. */
    def apply[T: LWeakTag]: LWeakTag[T] = implicitly
  }

  /**
    * The [[LTag]] of a type constructor with one type parameter, such as `Option`. Like `LTag`, it does not look for tags
    * of type parameters in scope: use it for type constructors known at the call site, and `TagK` in generic code. On
    * Scala 2 it is an alias of `LTag.StrongHK[{ type Arg[A] = K[A] }]`; on Scala 3, an alias of `LTag[K]`.
    *
    * {{{
    * import izumi.reflect.macrortti._
    *
    * val optionTag: LightTypeTag = LTagK[Option].tag
    * val sameAsTagK: Boolean = optionTag == izumi.reflect.TagK[Option].tag // true
    * }}}
    *
    * @see [[https://zio.dev/izumi-reflect/reference/ltag/ LTag and LTT]]
    */
  type LTagK[K[_]] = LTag.StrongHK[{ type Arg[A] = K[A] }]
  /** The `LTag` of a type constructor with two type parameters, such as `Either`. */
  type LTagKK[K[_, _]] = LTag.StrongHK[{ type Arg[A, B] = K[A, B] }]
  /** The `LTag` of a type constructor with three type parameters, such as `Function2`. */
  type LTagK3[K[_, _, _]] = LTag.StrongHK[{ type Arg[A, B, C] = K[A, B, C] }]

  /** The `LTag` of a type constructor whose one type parameter is itself a type constructor `F[_]`. */
  type LTagT[K[_[_]]] = LTag.StrongHK[{ type Arg[A[_]] = K[A] }]
  /** The `LTag` of a type constructor with type parameters `[F[_], A]`. */
  type LTagTK[K[_[_], _]] = LTag.StrongHK[{ type Arg[A[_], B] = K[A, B] }]
  /** The `LTag` of a type constructor with type parameters `[F[_], A, B]`. */
  type LTagTKK[K[_[_], _, _]] = LTag.StrongHK[{ type Arg[A[_], B, C] = K[A, B, C] }]
  /** The `LTag` of a type constructor with type parameters `[F[_], A, B, C]`. */
  type LTagTK3[K[_[_], _, _, _]] = LTag.StrongHK[{ type Arg[A[_], B, C, D] = K[A, B, C, D] }]

  /** Summoner of `LTagK`. */
  object LTagK {
    /** Returns the implicit `LTagK[K]` in scope, as in `LTagK[Option]`. */
    def apply[K[_]: LTagK]: LTagK[K] = implicitly
  }

  /** Summoner of `LTagKK`. */
  object LTagKK {
    /** Returns the implicit `LTagKK[K]` in scope. */
    def apply[K[_, _]: LTagKK]: LTagKK[K] = implicitly
  }

  /** Summoner of `LTagK3`. */
  object LTagK3 {
    /** Returns the implicit `LTagK3[K]` in scope. */
    def apply[K[_, _, _]: LTagK3]: LTagK3[K] = implicitly
  }

  /** Summoner of `LTagT`. */
  object LTagT {
    /** Returns the implicit `LTagT[K]` in scope. */
    def apply[K[_[_]]: LTagT]: LTagT[K] = implicitly
  }

  /** Summoner of `LTagTK`. */
  object LTagTK {
    /** Returns the implicit `LTagTK[K]` in scope. */
    def apply[K[_[_], _]: LTagTK]: LTagTK[K] = implicitly
  }

  /** Summoner of `LTagTKK`. */
  object LTagTKK {
    /** Returns the implicit `LTagTKK[K]` in scope. */
    def apply[K[_[_], _, _]: LTagTKK]: LTagTKK[K] = implicitly
  }

  /** Summoner of `LTagTK3`. */
  object LTagTK3 {
    /** Returns the implicit `LTagTK3[K]` in scope. */
    def apply[K[_[_], _, _, _]: LTagTK3]: LTagTK3[K] = implicitly
  }

  // simple materializers
  /**
    * Returns the [[LightTypeTag]] of `T`, built at compile time. It does not look for tags of type parameters: an abstract
    * type parameter stays a named placeholder, so `LTT[List[T]]` in a generic method does not describe the caller's type.
    * Use it for concrete types, and `izumi.reflect.Tag` in generic code.
    *
    * {{{
    * import izumi.reflect.macrortti._
    *
    * val listOfInt: LightTypeTag = LTT[List[Int]]
    * val same: Boolean = listOfInt == izumi.reflect.Tag[List[Int]].tag // true
    * }}}
    *
    * @see [[https://zio.dev/izumi-reflect/reference/ltag/ LTag and LTT]]
    */
  def LTT[T]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T]
  /** The variant of `LTT` for a type constructor `T[_]`, such as `List`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_]`[T[_]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor with a covariant parameter, `T[+_]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[+_]`[T[+_]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_ >: B <: A]` whose parameter has the lower bound `B` and the upper bound `A`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[A,B,_>:B<:A]`[A, B <: A, T[_ >: B <: A]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[B[x] <: A[x]]` whose parameter is a type constructor bounded by `A`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[A[_],_[_[x] <: A[x]]`[A[_], T[B[x] <: A[x]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_]]`[T[_[_]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[+_]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[+_]]`[T[_[+_]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[+_[_]]]`, whose type parameter is a higher-kinded constructor with a covariant parameter: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[+_[_]]]`[T[_[+_[_]]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_[_]]]`, whose type parameter is itself higher-kinded: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_[_]]]`[T[_[_[_]]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_, _]`, such as `Either`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_,_]`[T[_, _]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing, Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_, _]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_,_]]`[T[_[_, _]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[+_, +_]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[+_,+_]]`[T[_[+_, +_]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_, _], _, _]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_,_],_,_]`[T[_[_, _], _, _]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing, Nothing, Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[+_, +_], _, _]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[+_,+_],_,_]`[T[_[+_, +_], _, _]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing, Nothing, Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_], _[_]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_],_[_]]`[T[_[_], _[_]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing, Nothing]]
  /** The variant of `LTT` for a type constructor `T[_[_[_], _[_]]]`: returns its `LightTypeTag` as a type lambda. */
  def `LTT[_[_[_],_[_]]]`[T[_[_[_], _[_]]]]: LightTypeTag = macro LightTypeTagMacro.makeParsedLightTypeTag[T[Nothing]]
}
