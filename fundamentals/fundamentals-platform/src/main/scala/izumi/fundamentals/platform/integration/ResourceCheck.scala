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

package izumi.fundamentals.platform.integration

import izumi.fundamentals.platform.exceptions.IzThrowable._

sealed trait ResourceCheck

object ResourceCheck {

  final case class Success() extends ResourceCheck

  sealed trait Failure extends ResourceCheck

  final case class ResourceUnavailable(description: String, cause: Option[Throwable]) extends Failure {
    override def toString: String = {
      cause match {
        case Some(t) =>
          s"Unavailable resource: $description, ${t.getClass}: ${t.stackTrace}"

        case None =>
          s"Unavailable resource: $description"

      }
    }
  }

//  object ResourceUnavailable {
//    def apply(description: String, cause: Option[Throwable]): ResourceUnavailable = {
//      new ResourceUnavailable(description, cause)
//    }
//  }

}
