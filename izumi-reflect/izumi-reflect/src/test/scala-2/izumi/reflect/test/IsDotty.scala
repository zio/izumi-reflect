package izumi.reflect.test

import izumi.reflect.internal.fundamentals.platform.language.unused

import scala.language.implicitConversions

object IsDotty {
  implicit def isDotty(@unused t: this.type): Boolean = false
}
