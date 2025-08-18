package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

object TypeInspections {
  def apply(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): AbstractReference = {
    Inspector.make(qctx).buildTypeRef(typeRepr)
  }

  def unappliedDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[NameReference, Set[NameReference]] = {
    InheritanceDbInspector.make(qctx).makeUnappliedInheritanceDb(typeRepr)
  }

  def fullDb(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Map[AbstractReference, Set[AbstractReference]] = {
    FullDbInspector.make(qctx).buildFullDb(typeRepr)
  }

}
