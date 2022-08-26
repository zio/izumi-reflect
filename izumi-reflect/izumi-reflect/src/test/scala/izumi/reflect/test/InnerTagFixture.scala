package izumi.reflect.test

import izumi.reflect.Tag

// https://github.com/zio/izumi-reflect/issues/319
class InnerTagFixture {
  trait Make[T] {
    var tag: Any = null
    final def from[T: Tag](function: => T): Tag[T] = {
      val a = implicitly[Tag[T]]
      tag = a
      a
    }
  }

  class InnerTagIssue {
    val a = new Make[InnerTagIssue.this.type] {
      from(InnerTagIssue.this: InnerTagIssue.this.type)
    }
  }
}
