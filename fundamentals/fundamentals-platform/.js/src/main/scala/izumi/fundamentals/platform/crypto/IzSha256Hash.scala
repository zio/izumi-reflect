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

package izumi.fundamentals.platform.crypto

import izumi.fundamentals.platform
import izumi.fundamentals.platform.crypto

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSGlobal, JSImport}
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait ScalaJSSHA256 extends js.Any {
  def update(@deprecated("scalajs", "warnings") msg: Uint8Array): Unit = js.native
  def digest(@deprecated("scalajs", "warnings") enc: String): String = js.native
}

object ScalaJSSHA256 {
  @js.native
  @JSImport("hash.js", "sha256")
  class ImportedSHA256() extends js.Object with ScalaJSSHA256

  @js.native
  @JSGlobal("sha256")
  class GlobalSHA256() extends js.Object with ScalaJSSHA256
}

class IzSha256Hash(impl: () => ScalaJSSHA256) extends IzHash {
  override def hash(bytes: Array[Byte]): Array[Byte] = {
    val sha256 = impl()
    sha256.update(new Uint8Array(bytes.toJSArray))
    val hexdigest = sha256.digest("hex")
    hexdigest.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }
}

object IzSha256Hash {
  // scalajs fuckery
  private var impl: IzSha256Hash = Global
  def setImported(): Unit = synchronized {
    impl = Imported
  }
  def getImpl: IzSha256Hash = synchronized {
    impl
  }

  object Global extends IzSha256Hash(() => new crypto.ScalaJSSHA256.GlobalSHA256())
  object Imported extends IzSha256Hash(() => new platform.crypto.ScalaJSSHA256.ImportedSHA256())
}
