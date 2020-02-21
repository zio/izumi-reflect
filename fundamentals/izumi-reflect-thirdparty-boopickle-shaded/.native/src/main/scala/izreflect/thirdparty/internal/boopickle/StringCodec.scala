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

package izreflect.thirdparty.internal.boopickle

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object StringCodec extends StringCodecBase {
  override def decodeUTF8(len: Int, buf: ByteBuffer): String = {
    val a = new Array[Byte](len)
    buf.get(a)
    new String(a, StandardCharsets.UTF_8)
  }

  override def encodeUTF8(str: String): ByteBuffer = {
    ByteBuffer.wrap(str.getBytes(StandardCharsets.UTF_8))
  }

  override def decodeUTF16(len: Int, buf: ByteBuffer): String = {
    val a = new Array[Byte](len)
    buf.get(a)
    new String(a, StandardCharsets.UTF_16LE)
  }

  override def encodeUTF16(str: String): ByteBuffer = {
    ByteBuffer.wrap(str.getBytes(StandardCharsets.UTF_16LE))
  }
}
