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

package izumi.reflect.internal.fundamentals.platform.console

import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.{nowarn, tailrec}
import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config
import izumi.reflect.internal.fundamentals.platform.strings.IzString._

import scala.reflect.{ClassTag, classTag}

private[reflect] trait TrivialLogger {
  def log(s: => String): Unit
  def sub(): TrivialLogger = sub(1)
  def sub(delta: Int): TrivialLogger
}

private[reflect] trait AbstractStringTrivialSink {
  def flush(value: => String): Unit
}
private[reflect] object AbstractStringTrivialSink {
  private[reflect] object Console extends AbstractStringTrivialSink {
    override def flush(value: => String): Unit = System.out.println(value)
  }
}

private[reflect] final class TrivialLoggerImpl(config: Config, id: String, logMessages: Boolean, loggerLevel: Int) extends TrivialLogger {
  override def log(s: => String): Unit = {
    flush(format(s))
  }

  override def sub(delta: Int): TrivialLogger = {
    new TrivialLoggerImpl(config, id, logMessages, loggerLevel + delta)
  }

  @inline private[this] def format(s: => String): String = {
    s"$id: $s"
  }

  @inline private[this] def flush(s: => String): Unit = {
    if (logMessages) {
      config.sink.flush(s.shift(loggerLevel, "> "))
    }
  }
}

private[reflect] object TrivialLogger {
  private[reflect] final case class Config(
    sink: AbstractStringTrivialSink,
    forceLog: Boolean
  )
  private[reflect] object Config {
    private[reflect] lazy val console: Config = Config(sink = AbstractStringTrivialSink.Console, forceLog = false)
  }

  def make[T: ClassTag](config: Config): TrivialLogger = {
    val logMessages: Boolean = checkLog(config)
    new TrivialLoggerImpl(config, classTag[T].runtimeClass.getSimpleName, logMessages, loggerLevel = 0)
  }

  @inline private[this] def checkLog(config: Config): Boolean = {
    config.forceLog || enabled.get()
  }

  @nowarn("msg=return statement")
  private[this] val enabled: AtomicBoolean = {
    def prop(): Boolean = {
      val sysProperty = DebugProperties.`izumi.reflect.debug.macro.rtti` // this is the only debug logging property supported in the library
      val default = false

      val parts = sysProperty.split('.').toList

      @tailrec
      def check(current: String, tail: List[String]): Boolean = {
        if (System.getProperty(current).asBoolean().getOrElse(default)) {
          true
        } else {
          tail match {
            case ::(head, next) => check(s"$current.$head", next)
            case Nil => default
          }
        }
      }
      check(parts.head, parts.tail)
    }
    new AtomicBoolean(prop())
  }
  // for calling within a debugger or tests
  private[reflect] def enableLogs(): Unit = enabled.set(true)
  private[reflect] def disableLogs(): Unit = enabled.set(false)
  private[reflect] def statusLogs(): Boolean = enabled.get()
}
