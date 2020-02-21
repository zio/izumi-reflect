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

package izumi.fundamentals.platform.jvm

import java.lang.management.ManagementFactory
import java.net.{URLClassLoader, URLDecoder}
import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime

import scala.annotation.tailrec
import scala.concurrent.duration.Duration

trait IzJvm {

  import izumi.fundamentals.platform.time.IzTime._

  def uptime: Duration = Duration(getUptime, scala.concurrent.duration.MILLISECONDS)

  def startTime: ZonedDateTime = getStartTime.asEpochMillisUtc

  def isHeadless: Boolean = java.awt.GraphicsEnvironment.isHeadless

  def hasColorfulTerminal: Boolean = {
    val maybeTerm = Option(System.getenv("TERM"))
    maybeTerm.isDefined
  }

  def tempDir: Path = Paths.get(System.getProperty("java.io.tmpdir"))

  def terminalColorsEnabled: Boolean = {
    import izumi.fundamentals.platform.basics.IzBoolean._

    all(
      !isHeadless
      //hasColorfulTerminal, // idea doesn't set TERM :(
    )

  }

  protected def getUptime: Long = ManagementFactory.getRuntimeMXBean.getUptime

  protected def getStartTime: Long = ManagementFactory.getRuntimeMXBean.getStartTime

  @tailrec
  private def extractCp(classLoader: Option[ClassLoader], cp: Seq[String]): Seq[String] = {
    val clCp = classLoader match {
      case Some(u: URLClassLoader) =>
        u.getURLs
          .map(u => URLDecoder.decode(u.getFile, "UTF-8"))
          .toSeq
      case _ =>
        Seq.empty
    }

    val all = cp ++ clCp
    val parent = classLoader.flatMap(c => Option(c.getParent))
    parent match {
      case Some(cl) =>
        extractCp(Option(cl), all)
      case None =>
        all
    }
  }

  def safeClasspathSeq(classLoader: ClassLoader): Seq[String] = {
    val classLoaderCp = extractCp(Option(classLoader), Seq.empty)

    Seq(
      classLoaderCp,
      System.getProperty("java.class.path").split(':').toSeq
    ).flatten
  }

  def baseClassloader: ClassLoader = {
    Thread.currentThread.getContextClassLoader.getParent
  }

  def safeClasspath(classLoader: ClassLoader): String = {
    safeClasspathSeq(classLoader)
      .mkString(System.getProperty("path.separator"))
  }

  def safeClasspath(): String = safeClasspath(baseClassloader)

  def safeClasspathSeq(): Seq[String] = safeClasspathSeq(baseClassloader)
}

object IzJvm extends IzJvm
