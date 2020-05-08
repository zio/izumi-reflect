package izumi.reflect.test

import izumi.reflect.macrortti._

trait CurrentDottySupportExtentTest extends TagAssertions {

  trait Y
  trait Z extends Y
  trait XS[+K]
  trait X[+AAAAAAARG, -B0 <: Y] extends XS[AAAAAAARG] {}

  trait A
  trait B extends A

  trait Listoid[+K]

  trait Traitoid
  trait Invariantoid[V]
  trait SubInvariantoid[V] extends Invariantoid[V] with Traitoid

  // FIXME: report upstream: LTT macro calls do not work inside class body / inside normal test
  //  or even inside a method without a type signature (if you remove `: Unit` here, compilation will fail)
  def test(): Unit = {

    "super-basic test 1" in {
      val intTag = LTT[Int]

      class Foo[+F[_]]()
      class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
      class Baz() extends X[String, Z]

      val bazTag = LTT[Baz]
      val bazTag2 = LTT[Baz]

      val barXTag = LTT[Bar[X]]

      //      println(s"Baz tag: $bazTag")
      //      println(s"Bar[X] tag: $barXTag")
      //
      //      println(bazTag.debug())
      //      println(barXTag.debug())

      assertSame(bazTag, bazTag2)
      assertDifferent(bazTag, barXTag)
      assertChild(bazTag, bazTag2)
      assertNotChild(bazTag, barXTag)

      assertChild(LTT[B], LTT[A])


      val listTag = `LTT[_]`[Listoid]
      val listIntTag = LTT[Listoid[Int]]

      assertChild(listTag.combine(intTag), listIntTag)

      val listTag0 = `LTT[_]`[List]
      val listIntTag0 = LTT[List[Int]]

      assertChild(listTag0.combine(intTag), listIntTag0)


      val invTag0 = `LTT[_]`[SubInvariantoid]
      val invIntTag0 = LTT[Invariantoid[Int]]
      val combined = invTag0.combine(intTag)
      assertChild(combined, LTT[Traitoid])

//      println(invIntTag0.debug("invIntTag0"))
//      println(invTag0.debug("invTag0"))
//      assertChild(combined, invIntTag0)


    }

  }

  test()

}