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

package izumi.fundamentals.platform.time

import java.time.OffsetDateTime
import java.util.Date

import izumi.fundamentals.platform.time.IzTime.TZ_UTC

final class IzDate(private val value: Date) extends AnyVal {
  def toTsAsUtc: OffsetDateTime = value.toInstant.atZone(TZ_UTC).toOffsetDateTime

  def <=(other: Date): Boolean = {
    value.equals(other) || value.before(other)
  }

  def >=(other: Date): Boolean = {
    value.equals(other) || value.before(other)
  }

  def <(other: Date): Boolean = {
    value.before(other)
  }

  def >(other: Date): Boolean = {
    value.after(other)
  }
}
