package izumi.reflect.dottyreflection

import scala.quoted.QuoteContext

trait InspectorBase {
  // @formatter:off
  val qctx: QuoteContext
  given qctx.type = qctx
  import qctx.reflect.{given, _}
  // @formatter:on

  protected def shift: Int
  inline val debug = false

  protected def logStart(s: String) = {
    if (debug) println(" " * shift + s)
  }

  protected def log(s: String) = {
    if (debug) println(" " * shift + " -> " + s)
  }

  protected def logTpeAttrs[T](uns: TypeTree): Unit = {
    val symbol = uns.symbol
    if (debug) println(s"Attrs[$uns]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
  }
}
