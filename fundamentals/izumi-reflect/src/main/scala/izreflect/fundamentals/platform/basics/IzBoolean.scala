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

package izreflect.fundamentals.platform.basics

import scala.language.implicitConversions
import IzBoolean.LazyBool

private[izreflect] trait IzBoolean {
  @inline implicit final def toLazyBool(b: => Boolean): LazyBool = new LazyBool(() => b)

  @inline final def all(b1: Boolean, b: LazyBool*): Boolean = {
    b1 && b.forall(_.value)
  }

  @inline final def any(b1: Boolean, b: LazyBool*): Boolean = {
    b1 || b.exists(_.value)
  }
}

private[izreflect] object IzBoolean extends IzBoolean {
  @inline final implicit class LazyBool(private val b: () => Boolean) extends AnyVal {
    @inline def value: Boolean = b()
  }
}
