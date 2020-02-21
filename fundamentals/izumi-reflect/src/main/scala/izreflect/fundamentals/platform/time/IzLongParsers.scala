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

package izreflect.fundamentals.platform.time

import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}

import izreflect.fundamentals.platform.time.IzTime.TZ_UTC

final class IzLongParsers(private val t: Long) extends AnyVal {
  def asEpochSecondsLocal: LocalDateTime = {
    val instant = Instant.ofEpochSecond(t)
    LocalDateTime.ofInstant(instant, ZoneId.systemDefault())
  }

  def asEpochMillisLocal: LocalDateTime = {
    val instant = Instant.ofEpochMilli(t)
    LocalDateTime.ofInstant(instant, ZoneId.systemDefault())
  }

  def asEpochSecondsUtc: ZonedDateTime = {
    val instant = Instant.ofEpochSecond(t)
    ZonedDateTime.ofInstant(instant, TZ_UTC)
  }

  def asEpochMillisUtc: ZonedDateTime = {
    val instant = Instant.ofEpochMilli(t)
    ZonedDateTime.ofInstant(instant, TZ_UTC)
  }

}
