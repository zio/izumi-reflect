//package org.scalatest.wordspec
//
//abstract class AnyWordSpec extends App { self =>
//  export Predef.{assert, println => info}
//  export inner.{extension_in => extension_should}
//  export inner.extension_in
//
//  private object inner {
//    extension (s: String) def in(a: => Any): Unit = {
//      println(s)
//      a
//    }
//  }
//}
