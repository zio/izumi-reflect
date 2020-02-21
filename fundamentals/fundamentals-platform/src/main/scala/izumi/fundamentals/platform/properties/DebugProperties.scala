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

package izumi.fundamentals.platform.properties
import izumi.fundamentals.platform.strings.IzString._

/**
  * Marker trait for objects that hold names of Java Properties
  * that control debug output for macros and internals of `izumi` libraries.
  *
  * Search for inheritors of this trait to discover debugging java properties
  *
  * For example, to see macro debug output during compilation, set `-Dizumi.debug.macro.rtti=true` java property
  *
  * {{{
  *  sbt -Dizumi.debug.macro.rtti=true compile
  * }}}
  */
trait DebugProperties {
  case class Property protected (name: String) {
    def asBoolean(default: Boolean): Boolean = {
      System.getProperty(name).asBoolean().getOrElse(default)
    }
  }
}


