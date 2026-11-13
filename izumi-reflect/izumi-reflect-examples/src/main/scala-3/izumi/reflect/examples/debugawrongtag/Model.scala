package izumi.reflect.examples.debugawrongtag

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

final case class Registry(entries: Map[LightTypeTag, Any]) {
  def put[A: Tag](value: A): Registry = Registry(entries + (Tag[A].tag -> value))
  def get[A: Tag]: Option[A] = entries.get(Tag[A].tag).map(_.asInstanceOf[A])
}

trait Parent
trait Child extends Parent
