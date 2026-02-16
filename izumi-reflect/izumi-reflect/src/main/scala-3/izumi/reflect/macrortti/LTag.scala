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

final case class LTag[T <: AnyKind](tag: LightTypeTag)

/**
  * these are different summoners for light tags, it's fine for them to be the same structurally
  */
object LTag {
  def apply[T <: AnyKind: LTag]: LTag[T] = implicitly

  inline implicit def materialize[T <: AnyKind]: LTag[T] = LTag(izumi.reflect.dottyreflection.Inspect.inspectStrong[T])

  final case class Weak[T <: AnyKind](tag: LightTypeTag)
  object Weak {
    def apply[T <: AnyKind: LTag.Weak]: LTag.Weak[T] = implicitly

    inline implicit def materialize[T <: AnyKind]: LTag.Weak[T] = LTag.Weak(izumi.reflect.dottyreflection.Inspect.inspect[T])
  }

  type StrongHK[T <: AnyKind] = LTag[T]
  type WeakHK[T <: AnyKind] = LTag.Weak[T]
}
