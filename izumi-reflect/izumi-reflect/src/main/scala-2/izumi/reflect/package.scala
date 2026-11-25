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

package izumi

package object reflect {

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
  type TagK[K[_]] = HKTag[{ type Arg[A] = K[A] }]
  /** A tag for a type constructor with two type parameters, such as `Either` or `Map`. */
  type TagKK[K[_, _]] = HKTag[{ type Arg[A, B] = K[A, B] }]
  /** A tag for a type constructor with three type parameters, such as `Function2`. */
  type TagK3[K[_, _, _]] = HKTag[{ type Arg[A, B, C] = K[A, B, C] }]

  /** A tag for a type constructor whose one type parameter is itself a type constructor `F[_]`, such as a type class `Functor[F[_]]`. */
  type TagT[K[_[_]]] = HKTag[{ type Arg[A[_]] = K[A] }]
  /** A tag for a type constructor with type parameters `[F[_], A]`, such as a monad transformer `OptionT[F[_], A]`. */
  type TagTK[K[_[_], _]] = HKTag[{ type Arg[A[_], B] = K[A, B] }]
  /** A tag for a type constructor with type parameters `[F[_], A, B]`, such as `EitherT[F[_], A, B]`. */
  type TagTKK[K[_[_], _, _]] = HKTag[{ type Arg[A[_], B, C] = K[A, B, C] }]
  /** A tag for a type constructor with type parameters `[F[_], A, B, C]`. */
  type TagTK3[K[_[_], _, _, _]] = HKTag[{ type Arg[A[_], B, C, D] = K[A, B, C, D] }]

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
  type TagK4[K[_, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3] = K[A0, A1, A2, A3] }]
  /** A tag for a type constructor with 5 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK5[K[_, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4] = K[A0, A1, A2, A3, A4] }]
  /** A tag for a type constructor with 6 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK6[K[_, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5] = K[A0, A1, A2, A3, A4, A5] }]
  /** A tag for a type constructor with 7 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK7[K[_, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6] = K[A0, A1, A2, A3, A4, A5, A6] }]
  /** A tag for a type constructor with 8 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK8[K[_, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7] = K[A0, A1, A2, A3, A4, A5, A6, A7] }]
  /** A tag for a type constructor with 9 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK9[K[_, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8] }]
  /** A tag for a type constructor with 10 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK10[K[_, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9] }]
  /** A tag for a type constructor with 11 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK11[K[_, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] }]
  /** A tag for a type constructor with 12 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK12[K[_, _, _, _, _, _, _, _, _, _, _, _]] =
    HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] }]
  /** A tag for a type constructor with 13 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK13[K[_, _, _, _, _, _, _, _, _, _, _, _, _]] =
    HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] }]
  /** A tag for a type constructor with 14 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK14[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _]] =
    HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] }]
  /** A tag for a type constructor with 15 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK15[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] =
    HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] }]
  /** A tag for a type constructor with 16 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK16[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] =
    HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] }]
  /** A tag for a type constructor with 17 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK17[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]
  }]
  /** A tag for a type constructor with 18 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK18[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] =
      K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]
  }]
  /** A tag for a type constructor with 19 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK19[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] =
      K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]
  }]
  /** A tag for a type constructor with 20 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK20[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] =
      K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]
  }]
  /** A tag for a type constructor with 21 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK21[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] =
      K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]
  }]
  /** A tag for a type constructor with 22 type parameters. It has no companion object: summon it with `implicitly` or a context bound. */
  type TagK22[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{
    type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] =
      K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]
  }]

// TODO
//  type TagKUBound[U, K[_ <: U]] = HKTag[{ type Arg[A <: U] = K[A] }]
//  object TagKUBound {
//    def apply[U, K[_ <: U]](implicit ev: TagKUBound[U, K]): TagKUBound[U, K] = implicitly
//  }

}
