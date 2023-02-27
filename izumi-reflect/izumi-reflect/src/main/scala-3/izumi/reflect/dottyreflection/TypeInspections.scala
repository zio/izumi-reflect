package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import scala.quoted.{Quotes, Type}
import scala.collection.immutable.Queue

object TypeInspections {
  def apply[T <: AnyKind: Type](using qctx0: Quotes): AbstractReference = {
    Inspector.make(qctx0).buildTypeRef[T]
  }

  def unappliedDb[T <: AnyKind: Type](using qctx0: Quotes): Map[NameReference, Set[NameReference]] = {
    InheritanceDbInspector.make(qctx0).makeUnappliedInheritanceDb[T]
  }

  def fullDb[T <: AnyKind: Type](using qctx0: Quotes): Map[AbstractReference, Set[AbstractReference]] = {
    FullDbInspector.make(qctx0).buildFullDb[T]
  }

}
