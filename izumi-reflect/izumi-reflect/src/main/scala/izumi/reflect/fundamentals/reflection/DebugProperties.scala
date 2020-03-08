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

package izumi.reflect.fundamentals.reflection

/**
  * Java properties and macro settings that control behavior and debug output of Lightweight Reflection macros
  *
  * @see [[DebugProperties]]
  */
object DebugProperties {
  /**
    * To see macro debug output during compilation, set `-Dizumi.reflect.debug.macro.rtti=true` system property!
    *
    * {{{
    *   sbt -Dizumi.reflect.debug.macro.rtti=true compile
    * }}}
    */
  final val `izumi.reflect.debug.macro.rtti` = "izumi.reflect.debug.macro.rtti"

  /**
    * Add compiler option `-Xmacro-settings:izumi.reflect.rtti.cache.compile=false` to disable compile-time caching of computed
    * LightTypeTags. Caching is enabled by default for compile-time light type tag creation.
    *
    * {{{
    *   scalacOptions += "-Xmacro-settings:izumi.reflect.rtti.cache.compile=false"
    * }}}
    */
  final val `izumi.reflect.rtti.cache.compile` = "izumi.reflect.rtti.cache.compile"

  /**
    * Set system property `-Dizumi.reflect.rtti.cache.runtime=false` to disable caching for runtime creation of LightTypeTags.
    * Caching is enabled by default for runtime light type tag creation.
    *
    * {{{
    *   sbt -Dizumi.reflect.rtti.cache.runtime=false
    * }}}
    */
  final val `izumi.reflect.rtti.cache.runtime` = "izumi.reflect.rtti.cache.runtime"
}
