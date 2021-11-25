package izumi.reflect.test

import org.scalatest.exceptions.TestFailedException

trait TagProgressions { this: TagAssertions =>

  final def doesntWorkYetOnDotty(f: => Unit): Unit = {
    if (IsDotty) doesntWorkYet(f) else f
  }
  final def doesntWorkYetOnScala2(f: => Unit): Unit = {
    if (!IsDotty) doesntWorkYet(f) else f
  }
  final def doesntWorkYet(f: => Unit): Unit = {
    intercept[TestFailedException](f); ()
  }

  final def observableIncorrectBehaviorOnDottyButNotOnScala2(f: => Unit): Unit = {
    doesntWorkYetOnScala2(f)
  }

}
