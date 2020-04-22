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

import izumi.reflect.HKTag

package object reflect {

  /**
    * `TagK` is like a [[scala.reflect.api.TypeTags.TypeTag]] but for higher-kinded types.
    *
    * Example:
    * {{{
    * def containerTypesEqual[F[_]: TagK, K[_]: TagK]): Boolean = TagK[F].tag.tpe =:= TagK[K].tag.tpe
    *
    * containerTypesEqual[Set, collection.immutable.Set] == true
    * containerTypesEqual[Array, List] == false
    * }}}
    */
  type TagK[K[_]] = HKTag[{ type Arg[A] = K[A] }]
  type TagKK[K[_, _]] = HKTag[{ type Arg[A, B] = K[A, B] }]
  type TagK3[K[_, _, _]] = HKTag[{ type Arg[A, B, C] = K[A, B, C]}]

  type TagT[K[_[_]]] = HKTag[{ type Arg[A[_]] = K[A]}]
  type TagTK[K[_[_], _]] = HKTag[{ type Arg[A[_], B] = K[A, B] }]
  type TagTKK[K[_[_], _, _]] = HKTag[{ type  Arg[A[_], B, C] = K[A, B, C] }]
  type TagTK3[K[_[_], _, _, _]] = HKTag[{ type Arg[A[_], B, C, D] = K[A, B, C, D] }]

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

  type TagK4[K[_, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3] = K[A0, A1, A2, A3] } ]
  type TagK5[K[_, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4] = K[A0, A1, A2, A3, A4] } ]
  type TagK6[K[_, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5] = K[A0, A1, A2, A3, A4, A5] } ]
  type TagK7[K[_, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6] = K[A0, A1, A2, A3, A4, A5, A6] } ]
  type TagK8[K[_, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7] = K[A0, A1, A2, A3, A4, A5, A6, A7] } ]
  type TagK9[K[_, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8] } ]
  type TagK10[K[_, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9] } ]
  type TagK11[K[_, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] } ]
  type TagK12[K[_, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] } ]
  type TagK13[K[_, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] } ]
  type TagK14[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] } ]
  type TagK15[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] } ]
  type TagK16[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] } ]
  type TagK17[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] } ]
  type TagK18[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] } ]
  type TagK19[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] } ]
  type TagK20[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] } ]
  type TagK21[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] } ]
  type TagK22[K[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] = HKTag[{ type Arg[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = K[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] } ]

// TODO
//  type TagKUBound[U, K[_ <: U]] = HKTag[{ type Arg[A <: U] = K[A] }]
//  object TagKUBound {
//    def apply[U, K[_ <: U]](implicit ev: TagKUBound[U, K]): TagKUBound[U, K] = implicitly
//  }

}
