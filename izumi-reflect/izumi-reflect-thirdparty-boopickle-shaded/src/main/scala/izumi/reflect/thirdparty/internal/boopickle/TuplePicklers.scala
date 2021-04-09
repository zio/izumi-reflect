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

private[reflect] trait TuplePicklers extends PicklerHelper {

  implicit def Tuple2Pickler[T1: P, T2: P]: P[(T1, T2)] = new P[(T1, T2)] {
    override def pickle(x: (T1, T2))(implicit state: PickleState): Unit = { write[T1](x._1); write[T2](x._2) }
    override def unpickle(implicit state: UnpickleState): (T1, T2) = (read[T1], read[T2])
  }

}
