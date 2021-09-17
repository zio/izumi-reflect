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

/**
  * Specialized fast and cheap to initialize identity list for unpickle state identifier refs
  */
private[reflect] abstract class IdentList {
  @noinline def apply(idx: Int): AnyRef

  @noinline def updated(obj: AnyRef): IdentList
}

private[reflect] object IdentList {

  private[boopickle] final class Entry(val obj: AnyRef, var next: Entry)
  private[boopickle] val maxSize = 32
}

private[reflect] object EmptyIdentList extends IdentList {
  override def apply(idx: Int): AnyRef = throw new IndexOutOfBoundsException

  override def updated(obj: AnyRef): IdentList = new IdentList1Plus(obj)
}

/**
  * A fast and simple linked list implementation for identifier list
  *
  * @param o1 First object
  */
private[boopickle] final class IdentList1Plus(o1: AnyRef) extends IdentList {
  import IdentList.Entry
  var last: Entry = new Entry(o1, null)
  var head: Entry = last
  var switchOver = false
  var size = 0

  override def apply(idx: Int): AnyRef = {
    // first time something is looked up, we switch to the more efficient implementation
    switchOver = true
    var i = 0
    var e = head
    while (i < idx && e != null) {
      i += 1
      e = e.next
    }
    if (e == null)
      throw new IndexOutOfBoundsException
    e.obj
  }

  override def updated(obj: AnyRef): IdentList = {
    val e = new Entry(obj, null)
    last.next = e
    last = e
    size += 1
    if (switchOver || size > IdentList.maxSize)
      new IdentListBig(head, size)
    else
      this
  }
}

/**
  * A more scalable implementation for an identifier list
  *
  * @param first First entry in a list of entries
  */
private[boopickle] final class IdentListBig(first: IdentList.Entry, size: Int) extends IdentList {
  // transform the linked list into an array buffer
  val b = mutable.ArrayBuffer.newBuilder[AnyRef]
  b.sizeHint(size)
  var e = first
  while (e != null) {
    b += e.obj
    e = e.next
  }
  val entries = b.result()

  override def apply(idx: Int): AnyRef = {
    entries(idx)
  }

  override def updated(obj: AnyRef): IdentList = {
    entries += obj
    this
  }
}
