package izumi.reflect.test

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macrortti.LightTypeTag
import org.scalatest.wordspec.AnyWordSpec

trait TagLogging extends AnyWordSpec {

  // enable subtype comparison / .fromRuntime construction logging in tests
  def loud(f: => Any): Unit = {
    synchronized {
      TrivialLogger.enableLogs()
      try { f; () }
      finally TrivialLogger.disableLogs()
    }
  }

  def println(o: Any): Unit = info(o.toString)
  def println(o: LightTypeTag): Unit = info(o.ref.toString)

}
