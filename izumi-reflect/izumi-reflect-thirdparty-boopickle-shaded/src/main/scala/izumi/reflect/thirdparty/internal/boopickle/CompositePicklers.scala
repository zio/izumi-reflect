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

import scala.collection.mutable
import scala.reflect.ClassTag

/** Encodes a class belonging to a type hierarchy. Type is identified by the index in the `picklers` sequence, so care
  * must be taken to ensure picklers are added in the same order.
  */
private[reflect] class CompositePickler[A] extends Pickler[A] {

  import Constants._

  private var picklerClasses = IdentMap.empty
  private val picklers = mutable.ArrayBuffer.empty[(Class[_], Pickler[_])]

  override def pickle(obj: A)(implicit state: PickleState): Unit = {
    if (obj == null) {
      state.enc.writeInt(NullObject)
    } else {
      val clz = obj.getClass
      picklerClasses(clz) match {
        case None =>
          throw new IllegalArgumentException(s"This CompositePickler doesn't know class '${clz.getName}'.")
        case Some(idx) =>
          val pickler = picklers(idx - 2)._2
          state.enc.writeInt(idx - 1)
          pickler.asInstanceOf[Pickler[A]].pickle(obj)
      }
    }
  }

  override def unpickle(implicit state: UnpickleState): A = {
    val idx = state.dec.readInt
    if (idx == NullObject)
      null.asInstanceOf[A]
    else {
      if (idx < 0 || idx > picklers.size)
        throw new IllegalStateException(s"Index $idx is not defined in this CompositePickler")
      picklers(idx - 1)._2.asInstanceOf[Pickler[A]].unpickle
    }
  }

  private def addPickler[B](pickler: Pickler[B], tag: ClassTag[B]): Unit = {
    val clz = tag.runtimeClass
    if (picklerClasses(clz).isDefined)
      throw new IllegalArgumentException(s"Cannot add same class (${clz.getName}) twice to a composite pickler")
    picklerClasses = picklerClasses.updated(clz)
    picklers.append((clz, pickler))
  }

  @noinline def addConcreteType[B <: A](implicit pickler: Pickler[B], tag: ClassTag[B]): CompositePickler[A] = {
    addPickler(pickler, tag)
    this
  }

}
