package izumi.reflect.internal.fundamentals.platform.assertions

import izumi.reflect.DebugProperties
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString

import java.util.concurrent.atomic.AtomicBoolean

private[reflect] object IzAssert {
  private[reflect] final def apply(assertion: => Boolean): Unit = apply(assertion, "")
  private[reflect] final def apply(assertion: => Boolean, clue: => Any): Unit = {
    if (statusAsserts()) {
      if (!assertion) throw new IllegalArgumentException(s"IzAssert failed: $clue")
    }
  }

  /** caching is enabled by default for runtime light type tag creation */
  private[this] val enabled: AtomicBoolean = {
    val prop = System.getProperty(DebugProperties.`izumi.reflect.debug.macro.rtti.assertions`).asBoolean().getOrElse(false)
    new AtomicBoolean(prop)
  }
  // for calling within a debugger or tests
  private[reflect] def enableAsserts(): Unit = enabled.set(true)
  private[reflect] def disableAsserts(): Unit = enabled.set(false)
  private[reflect] def statusAsserts(): Boolean = enabled.get()
}
