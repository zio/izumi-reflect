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

private[reflect] class DecoderSize(val buf: ByteBuffer) extends Decoder {
  val stringCodec: StringCodecBase = StringCodec

  /** Decodes a single byte
    *
    * @return
    */
  def readByte: Byte = {
    buf.get
  }

  /** Decodes a 32-bit integer (1-5 bytes)
    * <pre>
    * 0XXX XXXX                            = 0 to 127
    * 1000 XXXX  b0                        = 128 to 4095
    * 1001 XXXX  b0                        = -1 to -4095
    * 1010 XXXX  b0 b1                     = 4096 to 1048575
    * 1011 XXXX  b0 b1                     = -4096 to -1048575
    * 1100 XXXX  b0 b1 b2                  = 1048576 to 268435455
    * 1101 XXXX  b0 b1 b2                  = -1048576 to -268435455
    * 1110 0000  b0 b1 b2 b3               = MinInt to MaxInt
    * 1111 ????                            = reserved for special codings
    * </pre>
    *
    * @return
    */
  def readInt: Int = {
    val b = buf.get & 0xFF
    if ((b & 0x80) != 0) {
      // special coding, expand sign bit
      val sign = if ((b & 0x10) == 0) 1 else -1
      val b0 = b & 0xF
      b >> 4 match {
        case 0x8 | 0x9 =>
          val b1 = buf.get & 0xFF
          sign * (b0 << 8 | b1)
        case 0xA | 0xB =>
          val b1 = buf.get & 0xFF
          val b2 = buf.get & 0xFF
          sign * (b0 << 16 | b1 << 8 | b2)
        case 0xC | 0xD =>
          val b1 = buf.get & 0xFF
          val b2 = buf.get & 0xFF
          val b3 = buf.get & 0xFF
          sign * (b0 << 24 | b1 << 16 | b2 << 8 | b3)
        case 0xE if b == 0xE0 =>
          sign * readRawInt
        case _ =>
          throw new IllegalArgumentException("Unknown integer coding")
      }
    } else {
      b
    }
  }

  def readRawInt: Int = {
    buf.getInt
  }

  /** Decodes a UTF-8 encoded string
    *
    * @return
    */
  def readString: String = {
    // read string length
    val len = readInt
    stringCodec.decodeFast(len, buf)
  }

  /** Decodes a UTF-8 encoded string whose length is already known
    *
    * @param len Length of the string (in bytes)
    * @return
    */
  def readString(len: Int): String = {
    stringCodec.decodeFast(len, buf)
  }

}

private[reflect] class EncoderSize(bufferProvider: BufferProvider = DefaultByteBufferProvider.provider) extends Encoder {
  val stringCodec: StringCodecBase = StringCodec

  @inline private def alloc(size: Int): ByteBuffer = bufferProvider.alloc(size)

  /** Encodes a single byte
    *
    * @param b Byte to encode
    * @return
    */
  def writeByte(b: Byte): Encoder = {
    alloc(1).put(b)
    this
  }

  /** Encodes an integer efficiently in 1 to 5 bytes
    * <pre>
    * 0XXX XXXX                            = 0 to 127
    * 1000 XXXX  b0                        = 128 to 4095
    * 1001 XXXX  b0                        = -1 to -4095
    * 1010 XXXX  b0 b1                     = 4096 to 1048575
    * 1011 XXXX  b0 b1                     = -4096 to -1048575
    * 1100 XXXX  b0 b1 b2                  = 1048575 to 268435455
    * 1101 XXXX  b0 b1 b2                  = -1048575 to -268435455
    * 1110 0000  b0 b1 b2 b3               = MinInt to MaxInt
    * 1111 ????                            = reserved for special codings
    * </pre>
    *
    * @param i Integer to encode
    */
  def writeInt(i: Int): Encoder = {
    // check for a short number
    if (i >= 0 && i < 128) {
      alloc(1).put(i.toByte)
    } else {
      if (i > -268435456 && i < 268435456) {
        val mask = i >>> 31 << 4
        val a = Math.abs(i)
        if (a < 4096) {
          alloc(2).put((mask | 0x80 | (a >> 8)).toByte).put((a & 0xFF).toByte)
        } else if (a < 1048576) {
          alloc(3).put((mask | 0xA0 | (a >> 16)).toByte).put(((a >> 8) & 0xFF).toByte).put((a & 0xFF).toByte)
        } else {
          alloc(4)
            .put((mask | 0xC0 | (a >> 24)).toByte)
            .put(((a >> 16) & 0xFF).toByte)
            .put(((a >> 8) & 0xFF).toByte)
            .put((a & 0xFF).toByte)
        }
      } else {
        alloc(5).put(0xE0.toByte).putInt(i)
      }
    }
    this
  }

  /** Encodes a string using UTF8
    *
    * @param s String to encode
    * @return
    */
  def writeString(s: String): Encoder = {
    writeInt(s.length)
    val bb = alloc(s.length * 3)
    stringCodec.encodeFast(s, bb)
    this
  }

  /** Completes the encoding and returns the ByteBuffer
    *
    * @return
    */
  def asByteBuffer: ByteBuffer = bufferProvider.asByteBuffer
}
