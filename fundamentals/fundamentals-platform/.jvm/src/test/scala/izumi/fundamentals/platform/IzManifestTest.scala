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

import izumi.fundamentals.platform.resources.IzManifest
import org.scalatest.wordspec.AnyWordSpec

class IzManifestTest extends AnyWordSpec {

  "Manifest reader" should {
    "support classpath manifest loading" in {
      val maybeMf = IzManifest.manifestCl("test.mf").map(IzManifest.read)
      assert(maybeMf.isDefined)
      val mf = maybeMf.get
      assert(mf.version.version.nonEmpty)
      assert(mf.git.revision.nonEmpty)
      assert(mf.git.branch.nonEmpty)
      assert(mf.git.repoClean)
      assert(mf.build.user.nonEmpty)
      assert(mf.build.jdk.nonEmpty)
      assert(mf.build.timestamp.getYear == 2018)
    }

    "support jar manifest loading" in {
      val maybeMf = IzManifest.manifest[AnyWordSpec]().map(IzManifest.read)
      assert(maybeMf.isDefined)
      val mf = maybeMf.get
      assert(mf.version.version.length > 2)
    }
  }


}
