package izumi.reflect.examples.heterogeneousmap

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

final case class TypedMap(entries: Map[LightTypeTag, Any]) {
  def put[A: Tag](value: A): TypedMap =
    TypedMap(entries + (Tag[A].tag -> value))

  def get[A: Tag]: Option[A] =
    entries.get(Tag[A].tag).map(_.asInstanceOf[A])
}

object TypedMap {
  val empty: TypedMap = TypedMap(Map.empty)
}
