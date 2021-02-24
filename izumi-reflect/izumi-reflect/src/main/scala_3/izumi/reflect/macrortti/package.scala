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

package object macrortti {
  type LWeakTag[T <: AnyKind] = LTag.Weak[T]
  object LWeakTag {
    def apply[T: LWeakTag]: LWeakTag[T] = implicitly
  }

  type LTagK[K[_]] = LTag[K]
  type LTagKK[K[_, _]] = LTag[K]
  type LTagK3[K[_, _, _]] = LTag[K]

  type LTagT[K[_[_]]] = LTag[K]
  type LTagTK[K[_[_], _]] = LTag[K]
  type LTagTKK[K[_[_], _, _]] = LTag[K]
  type LTagTK3[K[_[_], _, _, _]] = LTag[K]

  object LTagK {
    /**
    * Construct a type tag for a higher-kinded type `K[_]`
    *
    * Example:
    * {{{
    *     LTagK[Option]
    * }}}
    **/
    def apply[K[_]: LTagK]: LTagK[K] = implicitly
  }

  object LTagKK {
    def apply[K[_, _]: LTagKK]: LTagKK[K] = implicitly
  }

  object LTagK3 {
    def apply[K[_, _, _]: LTagK3]: LTagK3[K] = implicitly
  }

  object LTagT {
    def apply[K[_[_]]: LTagT]: LTagT[K] = implicitly
  }

  object LTagTK {
    def apply[K[_[_], _]: LTagTK]: LTagTK[K] = implicitly
  }

  object LTagTKK {
    def apply[K[_[_], _, _]: LTagTKK]: LTagTKK[K] = implicitly
  }

  object LTagTK3 {
    def apply[K[_[_], _, _, _]: LTagTK3]: LTagTK3[K] = implicitly
  }

//   simple materializers
  inline def LTT[T]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_]`[T[_]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[+_]`[T[+ _]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[A,B,_>:B<:A]`[A, B <: A, T[_ >: B <: A]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_]]`[T[_[_]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[+_]]`[T[_[+_]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_[_]]]`[T[_[_[_]]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  // Note that this variation is not required on Scala 2.13.5, a mismatch between versions on variance handling currently
  inline def `LTT[_[+_[_]]]`[T[_[+_[_]]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_,_]`[T[_, _]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_,_]]`[T[_[_, _]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[+_,+_]]`[T[_[+_, +_]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_,_],_,_]`[T[_[_, _], _, _]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[+_,+_],_,_]`[T[_[+_, +_], _, _]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_],_[_]]`[T[_[_], _[_]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
  inline def `LTT[_[_[_],_[_]]]`[T[_[_[_], _[_]]]]: LightTypeTag = dottyreflection.Inspect.inspect[T]
}
