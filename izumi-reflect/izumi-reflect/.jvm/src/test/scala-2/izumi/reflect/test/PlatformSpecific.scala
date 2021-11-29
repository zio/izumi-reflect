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

package izumi.reflect.test

import izumi.reflect.DebugProperties
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagImpl}
import scala.reflect.runtime.{universe => ru}

object PlatformSpecific {
  /** Use for stepping through in debugger */
  def fromRuntime[T: ru.TypeTag]: LightTypeTag = fromRuntime(ru.typeOf[T], false)
  def fromRuntime[T: ru.TypeTag](loud: Boolean): LightTypeTag = fromRuntime(ru.typeOf[T], loud)

  /** Use for stepping through in debugger */
  def fromRuntime(tpe: ru.Type, loud: Boolean): LightTypeTag = {
    synchronized {
      val doSet = loud && (System.getProperty(DebugProperties.`izumi.reflect.debug.macro.rtti`) eq null)
      if (!doSet) System.setProperty(DebugProperties.`izumi.reflect.debug.macro.rtti`, "true")
      try LightTypeTagImpl.makeLightTypeTag(ru)(tpe)
      finally {
        if (!doSet) System.clearProperty(DebugProperties.`izumi.reflect.debug.macro.rtti`); ()
      }
    }
  }
}
