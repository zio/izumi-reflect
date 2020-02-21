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

package izumi.functional

final class Value[A] private (private val value: A) extends AnyVal {
  @inline def map[B](f: A => B): Value[B] = {
    new Value(f(this.value))
  }

  @inline def mut[C, B](context: Option[C])(f: (C, A) => A): Value[A] = {
    context match {
      case Some(ctx) =>
        new Value(f(ctx, this.value))
      case None =>
        this
    }
  }

  @inline def mut[B](cond: Boolean)(f: A => A): Value[A] = {
    if (cond) {
      new Value(f(this.value))
    } else {
      this
    }
  }

  @inline def eff(f: A => Unit): Value[A] = {
    f(value)
    this
  }

  @inline def get: A = value
}

object Value {
  def apply[A](value: A): Value[A] = new Value[A](value)
}
