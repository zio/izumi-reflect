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

package izumi.fundamentals.platform.os

import java.io.File
import java.util.regex.Pattern

sealed trait OsType

object OsType {
  sealed trait Nix {
    this: OsType =>
  }

  case object Mac extends OsType with Nix

  case object Linux extends OsType with Nix

  case object Windows extends OsType

  case object Unknown extends OsType

}

object IzOs {
  def path: Seq[String] = {
    Option(System.getenv("PATH"))
      .map(_.split(Pattern.quote(File.pathSeparator)).toSeq)
      .toSeq
      .flatten
  }

  def osType: OsType = {
    System.getProperty("os.name").toLowerCase match {
      case s if s.contains("windows") =>
        OsType.Windows
      case s if s.contains("darwin") || s.contains("mac") =>
        OsType.Mac
      case s if s.contains("linux") =>
        OsType.Linux
      case _ =>
        OsType.Unknown
    }
  }
}
