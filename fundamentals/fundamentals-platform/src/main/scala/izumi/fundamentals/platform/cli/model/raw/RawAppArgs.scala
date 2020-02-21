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

package izumi.fundamentals.platform.cli.model.raw

import java.io.File
import java.nio.file.Path

import izumi.fundamentals.platform.cli.model.schema.ParserDef.ArgDef

final case class RawAppArgs(
                             globalParameters: RawEntrypointParams,
                             roles: Vector[RawRoleParams]
                           )

object RawAppArgs {
  def empty: RawAppArgs = RawAppArgs(RawEntrypointParams.empty, Vector.empty)
}

final case class RawRoleParams(role: String, roleParameters: RawEntrypointParams, freeArgs: Vector[String])

object RawRoleParams {
  def apply(role: String): RawRoleParams = RawRoleParams(role, RawEntrypointParams.empty, Vector.empty)
}

final case class RawEntrypointParams(flags: Vector[RawFlag], values: Vector[RawValue]) {
  def findValue(parameter: ArgDef): Option[RawValue] = values.find(parameter.name matches _.name)
  def findValues(parameter: ArgDef): Vector[RawValue] = values.filter(parameter.name matches _.name)
  def hasFlag(parameter: ArgDef): Boolean = flags.exists(parameter.name matches _.name)
  def hasNoFlag(parameter: ArgDef): Boolean = !hasFlag(parameter)
}

object RawEntrypointParams {
  def empty: RawEntrypointParams = RawEntrypointParams(Vector.empty, Vector.empty)
}

final case class RawFlag(name: String)

final case class RawValue(name: String, value: String)

object RawValue {
  implicit final class ValueExt(val value: RawValue) extends AnyVal {
    def asFile: File = new File(value.value)

    def asPath: Path = asFile.toPath

    def asString: String = value.value
  }

  implicit final class MaybeValueExt(val value: Option[RawValue]) extends AnyVal {
    def asFile: Option[File] = value.map(_.asFile)

    def asPath: Option[Path] = asFile.map(_.toPath)

    def asString: Option[String] = value.map(_.value)
  }
}
