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

package izreflect.fundamentals.platform.strings

class IzEscape(_toEscape: Set[Char], escapeChar: Char) {
  private val toEscape = _toEscape + escapeChar

  def escape(string: String): String = {
    val out = new StringBuilder()

    for (i <- string.indices) {
      val c = string.charAt(i)
      if (toEscape.contains(c)) {
        out.append(escapeChar)
        out.append(c)
      } else {
        out.append(c)
      }
    }


    out.toString()
  }

  def unescape(string: String): String = {
    val out = new StringBuilder()

    var inEscape = false

    for (i <- string.indices) {
      val c = string.charAt(i)
      if (inEscape) {
        out.append(c)
        inEscape = false
      } else if (c == escapeChar) {
        inEscape = true
      } else {
        out.append(c)
      }
    }

    out.toString()
  }

}
