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

import java.nio.file.Paths

import izumi.fundamentals.platform.files.IzZip
import izumi.fundamentals.platform.jvm.IzJvm
import org.scalatest.wordspec.AnyWordSpec

class IzZipTest extends AnyWordSpec {

  "zip tools" should {
    "be able to find files in jars" in {
      val files = IzJvm.safeClasspathSeq().map(p => Paths.get(p).toFile)

      for (_ <- 1 to 2) {
        val maybeObjContent = IzZip.findInZips(files, {
          p =>
            p.toString == Paths.get("/scala/Predef.class").toString
        })
        assert(maybeObjContent.headOption.exists(_._2.nonEmpty))
      }
    }
  }


}


