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

import java.time.ZonedDateTime
import java.time.temporal.TemporalAccessor
import java.util.concurrent.TimeUnit

import izumi.fundamentals.platform.time.IzTime.{ISO_DATE, ISO_DATE_TIME_3NANO}

import scala.concurrent.duration.{Duration, FiniteDuration}

final class IzTimeParsers(private val s: String) extends AnyVal {
  def toFiniteDuration: FiniteDuration = FiniteDuration(Duration(s).toNanos, TimeUnit.NANOSECONDS)

  def toTemporal: TemporalAccessor = ISO_DATE_TIME_3NANO.parse(s)

  def toDate: TemporalAccessor = ISO_DATE.parse(s)

  def toTsZ: ZonedDateTime = ZonedDateTime.parse(s, ISO_DATE_TIME_3NANO)
}

