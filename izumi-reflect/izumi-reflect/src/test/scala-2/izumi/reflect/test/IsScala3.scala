package izumi.reflect.test

import izumi.reflect.internal.fundamentals.platform.language.unused

import scala.language.implicitConversions

object IsScala3 {
  implicit def IsScala3(@unused t: this.type): Boolean = false
}
