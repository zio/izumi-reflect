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

import java.time._
import java.util.Date

trait IzTimeOrderingSafe {
  implicit val offsetDateTimeOrdering: Ordering[OffsetDateTime] = Ordering.fromLessThan(_ isBefore _)

  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)

  implicit val instantDateTimeOrdering: Ordering[Instant] = Ordering.fromLessThan(_ isBefore _)

  implicit val dateOrdering: Ordering[Date] = Ordering.fromLessThan(_ before _)

  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isBefore _)

  implicit val localTimeOrdering: Ordering[LocalTime] = Ordering.fromLessThan(_ isBefore _)

  implicit val offsetTimeOrdering: Ordering[OffsetTime] = Ordering.fromLessThan(_ isBefore _)
}

trait IzTimeOrdering extends IzTimeOrderingSafe {
  implicit val zonedDateTimeOrdering: Ordering[ZonedDateTime] = Ordering.fromLessThan(_ isBefore _)
}
