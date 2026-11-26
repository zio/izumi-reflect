package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

/** Entry points for inspecting a `TypeRepr`. */
object TypeInspections {
  /** Returns the `AbstractReference` of `typeRepr`. */
  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    Inspector.make(qctx).buildTypeRef(typeRepr)
  }

  /** Returns the inheritance database of unapplied type names of `typeRepr`. */
  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
  }

  /** Returns the bases database of `typeRepr`. */
  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    FullDbInspector.make(qctx).buildFullDb(typeRepr)
  }

}
