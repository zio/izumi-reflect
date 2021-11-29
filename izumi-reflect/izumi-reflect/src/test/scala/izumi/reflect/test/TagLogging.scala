package izumi.reflect.test

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macrortti.LightTypeTag
import org.scalatest.wordspec.AnyWordSpec

trait TagLogging extends AnyWordSpec {

  // enable logging in tests by default
  locally {
    TrivialLogger.enableLogs()
  }

  def println(o: Any): Unit = info(o.toString)
  def println(o: LightTypeTag): Unit = info(o.ref.toString)

}
