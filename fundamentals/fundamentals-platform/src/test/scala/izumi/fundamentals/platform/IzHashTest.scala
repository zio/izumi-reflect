
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

import org.scalatest.wordspec.AnyWordSpec

class IzHashTest extends AnyWordSpec {

//  "murmur32 hash" should {
//    "produce hex output" in {
//      import izumi.fundamentals.platform.crypto.IzMurmur32Hash
//      assert(IzMurmur32Hash.hash("abc") == "88ab65b3")
//    }
//  }

  "sha256 hash" should {
    "produce hex output" in {
      import izumi.fundamentals.platform.crypto.IzSha256Hash
      IzSha256Hash.setImported()
      for (_ <- 0 to 2) {
        assert(IzSha256Hash.getImpl.hash("abc") == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
      }
    }
  }
}

