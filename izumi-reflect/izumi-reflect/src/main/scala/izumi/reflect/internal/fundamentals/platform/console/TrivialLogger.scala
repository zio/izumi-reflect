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

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger.Config
import izumi.reflect.internal.fundamentals.platform.strings.IzString._

import scala.collection.mutable
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

private[reflect] final class TrivialLoggerImpl(config: Config, id: String, logMessages: Boolean, logErrors: Boolean, loggerLevel: Int) extends TrivialLogger {
  override def log(s: => String): Unit = {
    flush(format(s))
  }

  override def sub(delta: Int): TrivialLogger = {
    new TrivialLoggerImpl(config, id, logMessages, logErrors, loggerLevel + delta)
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
