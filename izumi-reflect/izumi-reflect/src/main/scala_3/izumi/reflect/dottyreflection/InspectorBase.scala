package izumi.reflect.dottyreflection

import scala.quoted.QuoteContext

trait InspectorBase {
  // @formatter:off
  val qctx: QuoteContext
  given as qctx.type = qctx
  import qctx.tasty.{Type => TType, given _, _}
  // @formatter:on

  protected def shift: Int

  protected def logStart(s: String) = {
    //println(" " * shift + s)
  }

  protected def log(s: String) = {
    //println(" " * shift + " -> " + s)
  }

  protected def logTpeAttrs[T](uns: TypeTree): Unit = {
    val symbol = uns.symbol
    println(s"Attrs[$uns]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
  }
}
