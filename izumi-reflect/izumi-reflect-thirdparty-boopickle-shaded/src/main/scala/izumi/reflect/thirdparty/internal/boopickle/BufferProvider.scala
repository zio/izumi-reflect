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

import java.nio.{ByteBuffer, ByteOrder}

private[reflect] trait BufferProvider {

  /**
    * Makes sure the ByteBuffer has enough space for new data. If not, allocates a new ByteBuffer
    * and returns it. The returned ByteBuffer must have little-endian ordering.
    *
    * @param size Number of bytes needed for new data
    * @return
    */
  def alloc(size: Int): ByteBuffer

  /**
    * Completes the encoding and returns the ByteBuffer, merging the chain of buffers if necessary
    *
    * @return
    */
  def asByteBuffer: ByteBuffer
}

private[reflect] abstract class ByteBufferProvider extends BufferProvider {
  import ByteBufferProvider._
  protected val pool = BufferPool
  protected var buffers: List[ByteBuffer] = Nil
  protected var currentBuf: ByteBuffer = allocate(initSize)

  protected def allocate(size: Int): ByteBuffer

  final private def newBuffer(size: Int): Unit = {
    // flip current buffer (prepare for reading and set limit)
    (currentBuf: java.nio.Buffer).flip()
    buffers = currentBuf :: buffers
    // replace current buffer with the new one, align to 16-byte border for small sizes
    currentBuf = allocate((math.max(size, expandSize) & ~15) + 16)
  }

  @inline final def alloc(size: Int): ByteBuffer = {
    if (currentBuf.remaining() < size)
      newBuffer(size)
    currentBuf
  }

  def asByteBuffer = {
    (currentBuf: java.nio.Buffer).flip()
    if (buffers.isEmpty) currentBuf
    else {
      val bufList = (currentBuf :: buffers).reverse
      // create a new buffer and combine all buffers into it
      val comb = allocate(bufList.map(_.limit()).sum)
      bufList.foreach(buf => comb.put(buf))
      (comb: java.nio.Buffer).flip()
      comb
    }
  }
}

private[reflect] object ByteBufferProvider {
  final val initSize = 512
  final val expandSize = initSize * 8
}

private[reflect] class HeapByteBufferProvider extends ByteBufferProvider {
  override protected def allocate(size: Int) = {
    if (pool.isDisabled)
      ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN)
    else
      pool.allocate(size).getOrElse(ByteBuffer.allocate(size).order(ByteOrder.LITTLE_ENDIAN))
  }

  override def asByteBuffer = {
    (currentBuf: java.nio.Buffer).flip()
    if (buffers.isEmpty) currentBuf
    else {
      // create a new buffer and combine all buffers into it
      val bufList = (currentBuf :: buffers).reverse
      val comb = allocate(bufList.map(_.limit()).sum)
      bufList.foreach {
        buf =>
          // use fast array copy
          java.lang.System.arraycopy(buf.array(), buf.arrayOffset(), comb.array(), comb.position(), buf.limit())
          (comb: java.nio.Buffer).position(comb.position() + buf.limit())
          // release to the pool
          pool.release(buf)
      }
      (comb: java.nio.Buffer).flip()
      comb
    }
  }
}

private[reflect] trait DefaultByteBufferProviderFuncs {
  def provider: ByteBufferProvider
}
