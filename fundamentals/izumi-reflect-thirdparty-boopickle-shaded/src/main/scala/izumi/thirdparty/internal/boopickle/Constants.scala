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

package izumi.thirdparty.internal.boopickle

private[boopickle] object Constants {
  final val NullRef    = -1
  final val NullObject = 0

  // codes for special strings
  final val StringInt: Byte       = 1
  final val StringLong: Byte      = 2
  final val StringUUID: Byte      = 3
  final val StringUUIDUpper: Byte = 4

  // codes for special Durations
  final val DurationInf: Byte       = 1
  final val DurationMinusInf: Byte  = 2
  final val DurationUndefined: Byte = 3

  // codes for Either
  final val EitherLeft: Byte  = 1
  final val EitherRight: Byte = 2

  // codes for Option
  final val OptionNone: Byte = 1
  final val OptionSome: Byte = 2

  // common strings that can be used as references
  val immutableInitData = Seq("null", "true", "false", "0", "1", "-1", "2", "3", "4", "5", "6", "7", "8", "9")

  val identityInitData = Seq(None)
}
