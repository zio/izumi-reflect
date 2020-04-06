//package izumi.reflect.dottyreflection
//
//object Main {
//
//  trait Y
//  trait Z extends Y
//  trait XS[+K]
//  trait X[+AAAAAAARG, -B <: Y] extends XS[AAAAAAARG] {}
//
//  trait A
//  trait B extends A
//
//  trait Listoid[+K]
//
//  def main(args: Array[String]): Unit = {
//    class Foo[+F[_]]()
//    class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
//    class Baz() extends X[String, Z]
//
//    val bazTag = Inspect.inspect[Baz]
//    val bazTag2 = Inspect.inspect[Baz]
//
//    val barXTag = Inspect.inspect[Bar[X]]
//
//    println(s"Baz tag: ${bazTag}")
//    println(s"Bar[X] tag: ${barXTag}")
//
//    println(bazTag.debug())
//    println(barXTag.debug())
//
//    println(assert(bazTag =:= bazTag2))
//    println(assert(!(bazTag =:= barXTag)))
//    println(assert(bazTag <:< bazTag2))
//    println(assert(!(bazTag <:< barXTag)))
//
//    println(assert(Inspect.inspect[B] <:<  Inspect.inspect[A]))
//
//    val listTag = Inspect.inspect[Listoid]
//    val listTag0 = Inspect.inspect[List]
//    val intTag = Inspect.inspect[Int]
//    val listIntTag = Inspect.inspect[Listoid[Int]]
//    val listIntTag0 = Inspect.inspect[List[Int]]
//
//    println(assert(listTag.combine(intTag) <:< listIntTag))
//
//    println("DONE")
//  }
//
//}
