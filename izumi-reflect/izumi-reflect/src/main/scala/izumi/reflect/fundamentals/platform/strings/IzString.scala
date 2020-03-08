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

package izumi.reflect.fundamentals.platform.strings

import scala.language.implicitConversions
import scala.util.Try

private[reflect] final class IzString(private val s: String) extends AnyVal {
  @inline final def asBoolean(): Option[Boolean] = {
    Try(s.toBoolean).toOption
  }

  @inline final def shift(delta: Int, fill: String = " "): String = {
    val shift = fill * delta
    s.split("\\\n", -1).map(s => s"$shift$s").mkString("\n")
  }
}

private[reflect] final class IzIterable[A](private val s: Iterable[A]) extends AnyVal {
  def niceList(shift: String = " ", prefix: String = "- "): String = {
    if (s.nonEmpty) {
      val fullPrefix = s"\n$shift$prefix"
      s.mkString(fullPrefix, fullPrefix, "")
    } else {
      "ø"
    }
  }
}

private[reflect] object IzString {
  private[reflect] implicit def toRichString(s: String): IzString = new IzString(s)
  private[reflect] implicit def toRichIterable[A](s: Iterable[A]): IzIterable[A] = new IzIterable(s)
}
