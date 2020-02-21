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

package izumi.fundamentals.platform.resources

import java.time.ZonedDateTime

import izumi.fundamentals.platform.time.IzTime._

case class IzArtifactId(groupId: String, artifactId: String) {
  override def toString: String = s"$groupId:$artifactId"
}

// TODO: full-scale version, with proper parsing & comparators
case class ArtifactVersion(version: String) {
  override def toString: String = version
}

case class BuildStatus(user: String, jdk: String, sbt: String, timestamp: ZonedDateTime) {
  override def toString: String = s"$user@${timestamp.isoFormatUtc}, JDK $jdk, SBT $sbt"
}

case class GitStatus(branch: String, repoClean: Boolean, revision: String) {
  override def toString: String = {
    val out = s"""$branch#$revision"""
    if (repoClean) {
      out
    } else {
      s"$out*"
    }
  }
}

case class IzArtifact(id: IzArtifactId, version: ArtifactVersion, build: BuildStatus, git: GitStatus) {
  def shortInfo: String = {
    s"$version @ $git, $id, ${build.timestamp.isoFormatUtc}"
  }

  def justVersion: String = {
    s"$version @ $git, ${build.timestamp.isoFormatUtc}"
  }

  override def toString: String = {
    s"$shortInfo (jdk: ${build.jdk}, by: ${build.user})"
  }
}
