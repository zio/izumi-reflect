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

import scala.concurrent.duration.Duration

final class IzDuration(private val duration: Duration) extends AnyVal {
  def readable: String = {
    val days = duration.toDays
    val hours = duration.toHours % 24
    val minutes = duration.toMinutes % 60
    val seconds = duration.toSeconds % 60
    s"${days}d, ${hours}h, ${minutes}m, ${seconds}s"
  }
}
