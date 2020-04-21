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
      config.sink.flush(s.shift(loggerLevel * 2))
    }
  }
}

private[reflect] object TrivialLogger {
  private[reflect] final case class Config(
                           sink: AbstractStringTrivialSink = AbstractStringTrivialSink.Console,
                           forceLog: Boolean = false
                         )

  def make[T: ClassTag](config: Config = Config()): TrivialLogger = {
    val logMessages: Boolean = checkLog(config)
    new TrivialLoggerImpl(config, classTag[T].runtimeClass.getSimpleName, logMessages, loggerLevel = 0)
  }

  @inline private[this] def checkLog(config: Config): Boolean = {
    config.forceLog || enabled.get()
  }

  private[this] val enabled: AtomicBoolean = {
    def prop(): Boolean = {
      val sysProperty = DebugProperties.`izumi.reflect.debug.macro.rtti` // this is the only debug logging property supported in the library
      val default = false

      val parts = sysProperty.split('.')
      var current = parts.head
      def cond: Boolean = {
        System.getProperty(current).asBoolean().getOrElse(default)
      }
      parts.tail.foreach {
        p =>
          if (cond) {
            return true
          } else {
            current = s"$current.$p"
          }
      }
      cond
    }
    new AtomicBoolean(prop())
  }
  // for calling within a debugger when live debugging
  def enableLogs(): Unit = enabled.set(true)
  def disableLogs(): Unit = enabled.set(true)
}
