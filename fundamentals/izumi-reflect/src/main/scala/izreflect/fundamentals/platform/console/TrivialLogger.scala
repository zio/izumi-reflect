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

package izreflect.fundamentals.platform.console

import izreflect.fundamentals.platform.console.TrivialLogger.{Config, Level}
import izreflect.fundamentals.platform.exceptions.IzThrowable._
import izreflect.fundamentals.platform.strings.IzString._

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

trait TrivialLogger {
  def log(s: => String): Unit
  def log(s: => String, e: => Throwable): Unit

  def err(s: => String): Unit
  def err(s: => String, e: => Throwable): Unit

  def sub(): TrivialLogger = sub(1)
  def sub(delta: Int): TrivialLogger
}

trait AbstractStringTrivialSink {
  def flush(value: => String): Unit
  def flushError(value: => String): Unit
}

object AbstractStringTrivialSink {
  object Console extends AbstractStringTrivialSink {
    override def flush(value: => String): Unit = System.out.println(value)
    override def flushError(value: => String): Unit = System.err.println(value)
  }
}

final class TrivialLoggerImpl(config: Config, id: String, logMessages: Boolean, logErrors: Boolean, loggerLevel: Int) extends TrivialLogger {
  override def log(s: => String): Unit = {
    flush(Level.Info, format(s))
  }

  override def log(s: => String, e: => Throwable): Unit = {
    flush(Level.Info, formatError(s, e))
  }

  override def err(s: => String): Unit = {
    flush(Level.Error, format(s))
  }

  override def err(s: => String, e: => Throwable): Unit = {
    flush(Level.Error, formatError(s, e))
  }

  override def sub(delta: Int): TrivialLogger = {
    new TrivialLoggerImpl(config, id, logMessages, logErrors, loggerLevel + delta)
  }

  @inline private[this] def format(s: => String): String = {
    s"$id: $s"
  }

  @inline private[this] def formatError(s: => String, e: => Throwable): String = {
    s"$id: $s\n${e.stackTrace}"
  }

  @inline private[this] def flush(level: Level, s: => String): Unit = {
    level match {
      case Level.Info =>
        if (logMessages) {
          config.sink.flush(s.shift(loggerLevel * 2))
        }
      case Level.Error =>
        if (logErrors) {
          config.sink.flushError(s.shift(loggerLevel * 2))
        }
    }
  }
}

object TrivialLogger {
  sealed trait Level
  object Level {
    case object Info extends Level
    case object Error extends Level
  }

  final case class Config(
                           sink: AbstractStringTrivialSink = AbstractStringTrivialSink.Console,
                           forceLog: Boolean = false
                         )

  def make[T: ClassTag](sysProperty: String, config: Config = Config()): TrivialLogger = {
    val logMessages: Boolean = checkLog(sysProperty, config, default = false)
    val logErrors: Boolean = checkLog(sysProperty, config, default = true)
    new TrivialLoggerImpl(config, classTag[T].runtimeClass.getSimpleName, logMessages, logErrors, loggerLevel = 0)
  }

  private[this] val enabled = new mutable.HashMap[String, Boolean]()

  private[this] def checkLog(sysProperty: String, config: Config, default: Boolean): Boolean = enabled.synchronized {
    config.forceLog || enabled.getOrElseUpdate(sysProperty, {
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
      return cond
    })
  }
}

