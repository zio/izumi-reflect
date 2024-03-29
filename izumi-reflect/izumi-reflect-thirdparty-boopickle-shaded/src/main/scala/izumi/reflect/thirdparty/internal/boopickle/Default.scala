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
import java.nio.charset.StandardCharsets

private[reflect] trait BasicImplicitPicklers extends PicklerHelper with XCompatImplicitPicklers {
  implicit def booleanPickler: P[Boolean] = BasicPicklers.BooleanPickler
  implicit def intPickler: P[Int] = BasicPicklers.IntPickler
  implicit def stringPickler: P[String] = BasicPicklers.StringPickler

  implicit def optionPickler[T: P]: P[Option[T]] = BasicPicklers.OptionPickler[T]
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

private[reflect] trait Base {
  type Pickler[A] = _root_.izumi.reflect.thirdparty.internal.boopickle.Pickler[A]
  type PickleState = _root_.izumi.reflect.thirdparty.internal.boopickle.PickleState
}

private[reflect] object NoMacro extends Base with BasicImplicitPicklers with TuplePicklers
