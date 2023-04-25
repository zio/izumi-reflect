package izumi.reflect.test

import izumi.reflect.macrortti._

trait CurrentDottySupportExtentTest extends TagAssertions {

  trait Y
  trait Z extends Y
  trait XS[+K]
  trait X[+AAAAAAARG, -B0] extends XS[AAAAAAARG] {}

  trait A
  trait B extends A

  trait Listoid[+K]

  trait Traitoid
  trait Invariantoid[V]
  trait SubInvariantoid[V] extends Invariantoid[V] with Traitoid

  "super-basic test 1" in { test() }

  // FIXME workaround for 'Cyclic reference involving class Bar'
  def test() = {
    val intTag = LTT[Int]

    class Foo[+F[_]]()
    class Bar[+F[+_, -_]]() extends Foo[F[Int, *]]
    class Baz() extends X[String, Z]

    val bazTag = LTT[Baz]
    val bazTag2 = LTT[Baz]

    val barXTag = LTT[Bar[X]]

    assertSame(bazTag, bazTag2)
    assertDifferent(bazTag, barXTag)
    assertChild(bazTag, bazTag2)
    assertNotChild(bazTag, barXTag)

    assertChild(LTT[B], LTT[A])

    val listTag = `LTT[_]`[Listoid]
    val listIntTag = LTT[Listoid[Int]]

    // see https://github.com/7mind/izumi/pull/1528
    assertSame(bazTag.combine(), bazTag2)

    assertChild(listTag.combine(intTag), listIntTag)

    val listTag0 = `LTT[_]`[List]
    val listIntTag0 = LTT[List[Int]]

    assertChild(listTag0.combine(intTag), listIntTag0)

    val invTag0 = `LTT[_]`[SubInvariantoid]
    val _ = LTT[Invariantoid[Int]]
    val combined = invTag0.combine(intTag)
    assertChild(combined, LTT[Traitoid])

    val tupleTag0 = LTT[(Any, Any)]
    val tupleTag1 = LTT[(Baz, Baz)]
    val tupleTag2 = LTT[(AnyVal, AnyVal)]
    val tupleTag3 = LTT[(Double, Double)]
    assertChild(tupleTag1, tupleTag0)
    assertChild(tupleTag3, tupleTag2)

    assertChild(LTT[List[B]], LTT[Seq[A]])
    assertChild(`LTT[_]`[List].combine(LTT[B]), `LTT[_]`[Seq].combine(LTT[A]))
  }

}
