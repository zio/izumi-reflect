package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.{Quotes, Type}

object TypeInspections {
  def apply[T <: AnyKind: Type](using qctx0: Quotes): AbstractReference = {
    new Inspector(0) { val qctx = qctx0 }.buildTypeRef[T]
  }

  def unappliedDb[T <: AnyKind: Type](using qctx0: Quotes): Map[NameReference, Set[NameReference]] = {
    new InheritanceDbInspector(0) { val qctx = qctx0 }.makeUnappliedInheritanceDb[T]
  }

  def fullDb[T <: AnyKind: Type](using qctx0: Quotes): Map[AbstractReference, Set[AbstractReference]] = {
    new FullDbInspector(0) { val qctx = qctx0 }.buildFullDb[T]
  }
}
