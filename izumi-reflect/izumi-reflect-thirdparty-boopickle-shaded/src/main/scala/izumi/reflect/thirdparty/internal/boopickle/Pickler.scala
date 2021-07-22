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
import scala.collection.mutable

private[reflect] trait Pickler[A] {
  def pickle(obj: A)(implicit state: PickleState): Unit
  def unpickle(implicit state: UnpickleState): A

  def xmap[B](ab: A => B)(ba: B => A): Pickler[B] = {
    val self = this
    new Pickler[B] {
      override def unpickle(implicit state: UnpickleState): B = {
        ab(self.unpickle(state))
      }
      override def pickle(obj: B)(implicit state: PickleState): Unit = {
        self.pickle(ba(obj))
      }
    }
  }
}

private[reflect] trait PicklerHelper {
  protected type P[A] = Pickler[A]

  /** Helper function to write pickled types
    */
  protected def write[A](value: A)(implicit state: PickleState, p: P[A]): Unit = p.pickle(value)(state)

  /** Helper function to unpickle a type
    */
  protected def read[A](implicit state: UnpickleState, u: P[A]): A = u.unpickle
}

private[reflect] object BasicPicklers extends PicklerHelper with XCompatPicklers {

  import Constants._

  object BooleanPickler extends P[Boolean] {
    @inline override def pickle(value: Boolean)(implicit state: PickleState): Unit = state.enc.writeByte(if (value) 1 else 0)
    @inline override def unpickle(implicit state: UnpickleState): Boolean = {
      state.dec.readByte match {
        case 1 => true
        case 0 => false
        case x => throw new IllegalArgumentException(s"Invalid value $x for Boolean")
      }
    }
  }

  object IntPickler extends P[Int] {
    @inline override def pickle(value: Int)(implicit state: PickleState): Unit = state.enc.writeInt(value)
    @inline override def unpickle(implicit state: UnpickleState): Int = state.dec.readInt
  }

  object StringPickler extends P[String] {
    override def pickle(s: String)(implicit state: PickleState): Unit = {
      state.immutableRefFor(s) match {
        case Some(idx) =>
          state.enc.writeInt(-idx)
        case None =>
          if (s.isEmpty) {
            state.enc.writeInt(0)
          } else {
            state.enc.writeString(s)
            state.addImmutableRef(s)
          }
      }
    }

    override def unpickle(implicit state: UnpickleState): String = {
      val len = state.dec.readInt
      if (len < 0) {
        state.immutableFor[String](-len)
      } else if (len == 0) {
        ""
      } else {
        val s = state.dec.readString(len)
        state.addImmutableRef(s)
        s
      }
    }
  }

  def OptionPickler[T: P]: P[Option[T]] = new P[Option[T]] {
    override def pickle(obj: Option[T])(implicit state: PickleState): Unit = {
      obj match {
        case null =>
          state.enc.writeInt(NullObject)
        case Some(x) =>
          state.enc.writeInt(OptionSome.toInt)
          write[T](x)
        case None =>
          // `None` is always encoded as zero
          state.enc.writeInt(OptionNone.toInt)
      }
    }

    override def unpickle(implicit state: UnpickleState): Option[T] = {
      state.dec.readInt match {
        case NullObject =>
          null
        case OptionSome =>
          val o = Some(read[T])
          o
        case OptionNone =>
          None
        case _ =>
          throw new IllegalArgumentException("Invalid coding for Option type")
      }
    }
  }

}

/** Manage state for a pickling "session".
  *
  * @param enc            Encoder instance to use
  * @param dedupImmutable Set to `false` if you want to disable deduplication of immutable values (like Strings)
  */
private[reflect] final class PickleState(val enc: Encoder, dedupImmutable: Boolean = true) {

  @inline def identityRefFor(obj: AnyRef): Option[Int] = None

  @inline def addIdentityRef(obj: AnyRef): Unit = ()

  /** Object reference for immutable pickled objects
    */
  private[this] var immutableRefs: mutable.AnyRefMap[AnyRef, Int] = null
  private[this] var immutableIdx = 2

  @inline def immutableRefFor(obj: AnyRef): Option[Int] = {
    if (obj == null)
      Some(1)
    else if (!dedupImmutable)
      None
    else if (immutableRefs != null)
      immutableRefs.get(obj)
    else
      None
  }

  @inline def addImmutableRef(obj: AnyRef): Unit = {
    if (dedupImmutable) {
      if (immutableRefs == null)
        immutableRefs = mutable.AnyRefMap.empty
      immutableRefs.update(obj, immutableIdx)
      immutableIdx += 1
    }
  }

  @inline def pickle[A](value: A)(implicit p: Pickler[A]): PickleState = {
    p.pickle(value)(this)
    this
  }

  def toByteBuffer: ByteBuffer = enc.asByteBuffer
}

private[reflect] object PickleState {

  /** Provides a default PickleState if none is available implicitly
    *
    * @return
    */
  implicit def pickleStateSpeed: PickleState = new PickleState(new EncoderSize, dedupImmutable = true)
}

/** Manage state for an unpickling "session"
  *
  * @param dec            Decoder instance to use
  * @param deduplicate    Set to `false` if you want to disable deduplication
  * @param dedupImmutable Set to `false` if you want to disable deduplication of immutable values (like Strings)
  */
private[reflect] final class UnpickleState(val dec: Decoder, deduplicate: Boolean = true, dedupImmutable: Boolean = true) {

  /** Object reference for pickled objects (use identity for equality comparison)
    *
    * Index 0 is not used
    * Index 1 = null
    * Index 2-n, references to pickled objects
    */
  private[this] var identityRefs: IdentList = EmptyIdentList

  @noinline def codingError(code: Int): Nothing = {
    throw new IllegalArgumentException(s"Unknown object coding: $code")
  }

  @noinline def identityFor[A <: AnyRef](ref: Int): A = {
    if (ref < 2)
      null.asInstanceOf[A]
    else if (!deduplicate)
      throw new Exception("Deduplication is disabled, but identityFor was called.")
    else
      identityRefs(ref - 2).asInstanceOf[A]
  }

  @inline def addIdentityRef(obj: AnyRef): Unit =
    if (deduplicate)
      identityRefs = identityRefs.updated(obj)

  /** Object reference for immutable pickled objects
    */
  private[this] var immutableRefs: IdentList = EmptyIdentList

  @noinline def immutableFor[A <: AnyRef](ref: Int): A = {
    if (ref < 2)
      null.asInstanceOf[A]
    else if (dedupImmutable)
      immutableRefs(ref - 2).asInstanceOf[A]
    else
      throw new Exception("Deduplication for immutable objects is disabled, but immutableFor was called.")
  }

  @inline def addImmutableRef(obj: AnyRef): Unit = {
    if (dedupImmutable)
      immutableRefs = immutableRefs.updated(obj)
  }

  @inline def unpickle[A](implicit u: Pickler[A]): A = u.unpickle(this)
}

private[reflect] object UnpickleState {
  def apply(bytes: ByteBuffer) = new UnpickleState(new DecoderSize(bytes))

  def apply(decoder: Decoder, deduplicate: Boolean = false, dedupImmutable: Boolean = false) =
    new UnpickleState(decoder, deduplicate)
}
