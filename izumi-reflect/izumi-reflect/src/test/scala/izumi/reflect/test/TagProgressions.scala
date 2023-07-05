package izumi.reflect.test

import org.scalatest.exceptions.TestFailedException

trait TagProgressions { this: TagAssertions =>

  final def brokenOnScala3(f: => Any): Unit = {
    if (IsScala3) broken(f) else f; ()
  }
  final def brokenOnScala2(f: => Any): Unit = {
    if (!IsScala3) broken(f) else f; ()
  }
  final def broken(f: => Any): Unit = {
    intercept[TestFailedException](f); ()
  }

}
