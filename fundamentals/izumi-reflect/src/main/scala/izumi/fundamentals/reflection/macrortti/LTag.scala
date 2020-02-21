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

package izumi.fundamentals.reflection.macrortti

import scala.language.experimental.macros

final case class LTag[T](tag: LightTypeTag)

/**
  * these are different summoners for light tags, it's fine for them to be the same structurally
  */
object LTag {
  def apply[T: LTag]: LTag[T] = implicitly

  implicit def materialize[T]: LTag[T] = macro LightTypeTagMacro.makeStrongTag[T]

  final case class Weak[T](tag: LightTypeTag)
  object Weak {
    def apply[T: LTag.Weak]: LTag.Weak[T] = implicitly

    implicit def materialize[T]: LTag.Weak[T] = macro LightTypeTagMacro.makeWeakTag[T]
  }

  final case class StrongHK[T](tag: LightTypeTag)
  object StrongHK {
    implicit def materialize[T]: LTag.StrongHK[T] = macro LightTypeTagMacro.makeStrongHKTag[T]
  }

  final case class WeakHK[T](tag: LightTypeTag)
  object WeakHK {
    implicit def materialize[T]: LTag.WeakHK[T] = macro LightTypeTagMacro.makeWeakHKTag[T]
  }
}
