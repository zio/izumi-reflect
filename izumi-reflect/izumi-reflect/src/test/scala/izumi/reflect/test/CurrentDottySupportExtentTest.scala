package izumi.reflect.test

import izumi.reflect.macrortti._

class CurrentDottySupportExtentTest extends TagAssertions {

  trait Y
  trait Z extends Y
  trait XS[+K]
  trait X[+AAAAAAARG, -B <: Y] extends XS[AAAAAAARG] {}

  trait A
  trait B extends A

  trait Listoid[+K]

  // FIXME: report upstream: LTT macro calls do not work inside class body / inside normal test
  //  or even inside a method without a type signature (if you remove `: Unit` here, compilation will fail)
  def test(): Unit = {

    "super-basic test 1" in {
      class Foo[+F[_]]()
      class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
      class Baz() extends X[String, Z]

      val bazTag = LTT[Baz]
      val bazTag2 = LTT[Baz]

      val barXTag = LTT[Bar[X]]

      println(s"Baz tag: $bazTag")
      println(s"Bar[X] tag: $barXTag")

      println(bazTag.debug())
      println(barXTag.debug())

      assertSame(bazTag, bazTag2)
      assertDifferent(bazTag, barXTag)
      assertChild(bazTag, bazTag2)
      assertNotChild(bazTag, barXTag)

      assertChild(LTT[B], LTT[A])

      val intTag = LTT[Int]

      val listTag = `LTT[_]`[Listoid]
      val listIntTag = LTT[Listoid[Int]]

      assertChild(listTag.combine(intTag), listIntTag)

      val listTag0 = `LTT[_]`[List]
      val listIntTag0 = LTT[List[Int]]

      assertChild(listTag0.combine(intTag), listIntTag0)
    }

  }

  test()

}