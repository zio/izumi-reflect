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

package izumi.reflect.thirdparty.internal.boopickle

import java.nio.ByteBuffer

private[reflect] trait Decoder {

  /** Decodes a single byte
    */
  def readByte: Byte

  /** Decodes a 32-bit integer
    */
  def readInt: Int

  /** Decodes a string
    */
  def readString: String

  /** Decodes a string whose length is already known
    *
    * @param len Length of the string (in bytes)
    */
  def readString(len: Int): String
}

private[reflect] trait Encoder {

  /** Encodes a single byte
    *
    * @param b Byte to encode
    */
  def writeByte(b: Byte): Encoder

  /** Encodes an integer
    */
  def writeInt(i: Int): Encoder

  /** Encodes a string
    *
    * @param s String to encode
    */
  def writeString(s: String): Encoder

  /** Completes the encoding and returns the ByteBuffer
    */
  def asByteBuffer: ByteBuffer
}
