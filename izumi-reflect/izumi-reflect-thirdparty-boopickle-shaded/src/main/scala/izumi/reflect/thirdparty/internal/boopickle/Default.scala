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

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.util.UUID
import scala.concurrent.duration.{Duration, FiniteDuration}
//import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.Try

private[reflect] trait BasicImplicitPicklers extends PicklerHelper with XCompatImplicitPicklers {
  implicit def unitPickler: ConstPickler[Unit] = BasicPicklers.UnitPickler
  implicit def booleanPickler: P[Boolean] = BasicPicklers.BooleanPickler
  implicit def bytePickler: P[Byte] = BasicPicklers.BytePickler
  implicit def shortPickler: P[Short] = BasicPicklers.ShortPickler
  implicit def charPickler: P[Char] = BasicPicklers.CharPickler
  implicit def intPickler: P[Int] = BasicPicklers.IntPickler
  implicit def longPickler: P[Long] = BasicPicklers.LongPickler
  implicit def floatPickler: P[Float] = BasicPicklers.FloatPickler
  implicit def doublePickler: P[Double] = BasicPicklers.DoublePickler
  implicit def byteBufferPickler: P[ByteBuffer] = BasicPicklers.ByteBufferPickler
  implicit def stringPickler: P[String] = BasicPicklers.StringPickler

  // less frequently used picklers are initializes lazily to enable DCE
  implicit lazy val bigIntPickler: P[BigInt] = BasicPicklers.BigIntPickler
  implicit lazy val bigDecimalPickler: P[BigDecimal] = BasicPicklers.BigDecimalPickler
  implicit lazy val UUIDPickler: P[UUID] = BasicPicklers.UUIDPickler
  implicit lazy val durationPickler: P[Duration] = BasicPicklers.DurationPickler
  implicit lazy val finiteDurationPickler: P[FiniteDuration] = BasicPicklers.FiniteDurationPickler
  implicit lazy val infiniteDurationPickler: P[Duration.Infinite] = BasicPicklers.InfiniteDurationPickler

  implicit def optionPickler[T: P]: P[Option[T]] = BasicPicklers.OptionPickler[T]
  implicit def somePickler[T: P]: P[Some[T]] = BasicPicklers.SomePickler[T]
  implicit def eitherPickler[T: P, S: P]: P[Either[T, S]] = BasicPicklers.EitherPickler[T, S]
  implicit def leftPickler[T: P, S: P]: P[Left[T, S]] = BasicPicklers.LeftPickler[T, S]
  implicit def rightPickler[T: P, S: P]: P[Right[T, S]] = BasicPicklers.RightPickler[T, S]
  implicit def arrayPickler[T: P: ClassTag]: P[Array[T]] = BasicPicklers.ArrayPickler[T]
}

private[reflect] object PickleImpl {
  @inline def apply[A](value: A)(implicit state: PickleState, p: Pickler[A]): PickleState = {
    p.pickle(value)(state)
    state
  }

  @inline def intoBytes[A](value: A)(implicit state: PickleState, p: Pickler[A]): ByteBuffer = {
    apply(value).toByteBuffer
  }

  @inline private[reflect] def serializeIntoString[A](a: A, pickler: Pickler[A]): String = {
    val pickleState = PickleState.pickleStateSpeed
    val buf = PickleImpl.intoBytes(a)(pickleState, pickler)
    new String(buf.array(), buf.arrayOffset(), buf.limit(), StandardCharsets.ISO_8859_1)
  }
}

private[reflect] object UnpickleImpl {
  def apply[A](implicit u: Pickler[A]) = UnpickledCurry(u)

  private[reflect] case class UnpickledCurry[A](u: Pickler[A]) {
    def apply(implicit state: UnpickleState): A = u.unpickle(state)

    def fromBytes(bytes: ByteBuffer)(implicit buildState: ByteBuffer => UnpickleState): A = {
      // keep original byte order
      val origByteOrder = bytes.order()
      // but decode as little-endian
      val result = u.unpickle(buildState(bytes.order(ByteOrder.LITTLE_ENDIAN)))
      bytes.order(origByteOrder)
      result
    }

    def tryFromBytes(bytes: ByteBuffer)(implicit buildState: ByteBuffer => UnpickleState): Try[A] = Try(fromBytes(bytes))

    def fromState(state: UnpickleState): A = u.unpickle(state)
  }

}

private[reflect] trait Base {
  type Pickler[A] = _root_.izumi.reflect.thirdparty.internal.boopickle.Pickler[A]
  def Pickle: PickleImpl.type = _root_.izumi.reflect.thirdparty.internal.boopickle.PickleImpl
  type PickleState = _root_.izumi.reflect.thirdparty.internal.boopickle.PickleState
  def Unpickle: UnpickleImpl.type = _root_.izumi.reflect.thirdparty.internal.boopickle.UnpickleImpl
  type UnpickleState = _root_.izumi.reflect.thirdparty.internal.boopickle.UnpickleState

  def compositePickler[A <: AnyRef]: CompositePickler[A] = CompositePickler[A]

  def exceptionPickler: CompositePickler[Throwable] = ExceptionPickler.base
}

private[reflect] object NoMacro extends Base with BasicImplicitPicklers with TuplePicklers
