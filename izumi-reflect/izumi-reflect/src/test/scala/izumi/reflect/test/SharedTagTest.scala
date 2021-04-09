package izumi.reflect.test

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag110
import izumi.reflect.macrortti.{LTag, LightTypeTag}
import izumi.reflect.test.ID.id
import izumi.reflect.thirdparty.internal.boopickle.{PickleImpl, PickleState, Pickler}
import org.scalatest.Assertions
import org.scalatest.exceptions.TestFailedException
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import scala.annotation.StaticAnnotation
import scala.util.Try

object ID {
  type id[A] = A
  type Identity[+A] = A
}

trait ZY extends Assertions {
  type T
  type U = T
  type V = List[T]
  type A = List[Option[Int]]
  val x: String = "5"
  object y

  val tagT = intercept[TestFailedException](assertCompiles("Tag[T]"))
  val tagU = intercept[TestFailedException](assertCompiles("Tag[U]"))
  val tagV = intercept[TestFailedException](assertCompiles("Tag[V]"))
  val tagA = Try(assertCompiles("Tag[A]"))
}

trait XY[Y] {
  type Z = id[Y]

  implicit def tagZ: Tag[Z]
}

abstract class SharedTagTest extends AnyWordSpec with XY[String] {

  import izumi.reflect.test.PlatformSpecific.fromRuntime

  def fromLTag[T: LTag]: LightTypeTag = LTag[T].tag

  final val str = "str"

  final class With[T] extends StaticAnnotation

  "Tag (including Dotty)" should {

    "Work for any concrete type" in {
      assert(Tag[Int].tag == fromRuntime[Int])
      assert(Tag[Set[String]].tag == fromRuntime[Set[String]])
      assert(Tag[Map[Boolean, Double]].tag == fromRuntime[Map[Boolean, Double]])
      assert(Tag[_ => Unit].tag == fromRuntime[_ => Unit])
      assert(Tag[Unit => _].tag == fromRuntime[Unit => _])
      assert(Tag[_ => _].tag == fromRuntime[_ => _])

      assert(Tag[Any].tag == fromRuntime[Any])
      assert(Tag[Nothing].tag == fromRuntime[Nothing])
      assert(Tag[Any => Nothing].tag == fromRuntime[Any => Nothing])
      assert(Tag[Nothing => Any].tag == fromRuntime[Nothing => Any])

      assert(Tag[With[Any]].tag == fromRuntime[With[Any]])
      assert(Tag[With[Nothing]].tag == fromRuntime[With[Nothing]])
      assert(Tag[With[_]].tag == fromRuntime[With[_]])

      assert(Tag[Int with String].tag == fromRuntime[Int with String])

      assert(Tag[str.type].tag == fromRuntime[str.type])
    }

    "regression test for https://github.com/zio/izumi-reflect/issues/98" in {
      object SomeService {
        trait Service[T]

        final case class Foo()

        val tag1: Tag[Service[Foo]] = Tag[Service[Foo]]
      }

      object IzumiReflectTagEqualRegression {
        import SomeService._

        def test(): Unit = {
          val tag1: Tag[Service[Foo]] = SomeService.tag1
          val tag2: Tag[Service[Foo]] = Tag[Service[Foo]]

          val rtTag1 = PickleImpl.serializeIntoString(tag1.tag.ref, LightTypeTag.lttRefSerializer)
          val rtTag2 = PickleImpl.serializeIntoString(tag2.tag.ref, LightTypeTag.lttRefSerializer)

          assert(tag1.tag.ref == tag2.tag.ref)

          assert(rtTag1 == tag1.tag.asInstanceOf[ParsedLightTypeTag110].refString)
          assert(rtTag2 == tag2.tag.asInstanceOf[ParsedLightTypeTag110].refString)

          assert(rtTag1 == rtTag2)

          assert(tag1.tag == tag2.tag)
        }
      }

      IzumiReflectTagEqualRegression.test()
    }


  }

}
