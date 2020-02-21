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

private[boopickle] object ReferenceEquality {
  @inline def eq(a: AnyRef, b: AnyRef): Boolean  = a eq b
  @inline def ne(a: AnyRef, b: AnyRef): Boolean  = a ne b
  @inline def identityHashCode(obj: AnyRef): Int = System.identityHashCode(obj)
}
