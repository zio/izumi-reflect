package izumi.reflect.test

import izumi.reflect.internal.fundamentals.platform.assertions.IzAssert
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macrortti.LightTypeTag
import org.scalatest.wordspec.AnyWordSpec

trait TagLogging extends AnyWordSpec {

  // enable subtype comparison / .fromRuntime construction logging in tests
  final def withDebugOutput(f: => Any): Unit = TagLogging.withDebugOutput { f; () }
  final def withSanityChecks(f: => Any): Unit = TagLogging.withSanityChecks { f; () }

  final def println(o: Any): Unit = info(o.toString)
  final def println(o: LightTypeTag): Unit = info(o.ref.toString)

}

object TagLogging {
  def withDebugOutput[T](f: => T): T = {
    synchronized {
      val enabledBefore = TrivialLogger.statusLogs()

      if (!enabledBefore) { TrivialLogger.enableLogs() }
      try {
        f
      } finally {
        if (!enabledBefore) { TrivialLogger.disableLogs() }
      }
    }
  }
  def withSanityChecks[T](f: => T): T = {
    synchronized {
      val enabledBefore = IzAssert.statusAsserts()
      if (!enabledBefore) { IzAssert.enableAsserts() }
      try {
        f
      } finally {
        if (!enabledBefore) { IzAssert.disableAsserts() }
      }
    }
  }
}
