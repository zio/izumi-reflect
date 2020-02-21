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

package izumi.fundamentals.platform

import java.util.concurrent.TimeUnit

import izumi.fundamentals.platform.files.IzFiles
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class IzFilesTest extends AnyWordSpec {

  "File tools" should {
    "resolve path entries on nix-like systems" in {
      assert(IzFiles.which("bash").nonEmpty)
    }
  }

  import izumi.fundamentals.platform.resources.IzResources
  import scala.concurrent.Future

  "Resource tools" should {
    "support concurrent queries" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      val seq = (0 to 200).map{
        _ =>
          Future(IzResources.getPath("reflect.properties"))
      }
      val res = Await.result(Future.sequence(seq), Duration.apply(1, TimeUnit.MINUTES))
      assert(res.forall(_.isDefined))
    }
  }

}
