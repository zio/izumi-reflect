package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.quoted.{QuoteContext, Type}

object TypeInspections {
  def apply[T <: AnyKind : Type](using qctx0: QuoteContext): AbstractReference = {
    new Inspector(0) { val qctx = qctx0 }.buildTypeRef[T]
  }

  def nameDb[T <: AnyKind : Type](using qctx0: QuoteContext): Map[NameReference, Set[NameReference]] = {
    new DbInspector(0) { val qctx = qctx0 }.buildNameDb[T]
  }

  def fullDb[T <: AnyKind : Type](using qctx0: QuoteContext): Map[AbstractReference, Set[AbstractReference]] = {
    new FullDbInspector(0) { val qctx = qctx0 }.buildFullDb[T]
  }
}
