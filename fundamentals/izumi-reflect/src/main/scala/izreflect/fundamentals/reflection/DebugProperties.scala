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

package izreflect.fundamentals.reflection

import izreflect.fundamentals.platform.properties

/**
  * Java properties and macro settings that control behavior and debug output of Lightweight Reflection macros
  *
  * @see [[DebugProperties]]
  */
object DebugProperties extends properties.DebugProperties {
  /**
    * To see macro debug output during compilation, set `-Dizumi.debug.macro.rtti=true` system property!
    *
    * {{{
    *   sbt -Dizumi.debug.macro.rtti=true compile
    * }}}
    */
  final val `izumi.debug.macro.rtti` = "izumi.debug.macro.rtti"

  /**
    * Add compiler option `-Xmacro-settings:izumi.rtti.cache.compile=false` to disable compile-time caching of computed
    * LightTypeTags. Caching is enabled by default for compile-time light type tag creation.
    *
    * {{{
    *   scalacOptions += "-Xmacro-settings:izumi.rtti.cache.compile=false"
    * }}}
    */
  final val `izumi.rtti.cache.compile` = "izumi.rtti.cache.compile"

  /**
    * Set system property `-Dizumi.rtti.cache.runtime=false` to disable caching for runtime creation of LightTypeTags.
    * Caching is enabled by default for runtime light type tag creation.
    *
    * {{{
    *   sbt -Dizumi.rtti.cache.runtime=false
    * }}}
    */
  final val `izumi.rtti.cache.runtime` = "izumi.rtti.cache.runtime"
}
