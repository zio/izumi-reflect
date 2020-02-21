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

package izreflect.fundamentals.platform.language

import scala.language.implicitConversions

/**
  * Syntax for explicitly discarding values to satisfy -Ywarn-value-discard,
  * and for clarity of course!
  *
  * @see also [[izreflect.fundamentals.platform.language.unused]]
  **/
object Quirks {

  @inline final def discard(@unused trash: Any*): Unit = ()

  @inline final def forget(@unused trash: LazyDiscarder[_]*): Unit = ()

  @inline implicit final class Discarder[T](private val t: T) extends AnyVal {
    @inline def discard(): Unit = ()
  }

  @inline implicit final def LazyDiscarder[T](@unused t: => T): LazyDiscarder[Unit] = new LazyDiscarder[Unit]()

  @inline final class LazyDiscarder[U >: Unit](private val dummy: Boolean = false) extends AnyVal {
    @inline def forget: U = ()
  }

}
