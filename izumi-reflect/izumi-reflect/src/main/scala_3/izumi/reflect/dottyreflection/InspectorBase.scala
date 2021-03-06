package izumi.reflect.dottyreflection

import scala.quoted.Quotes

trait InspectorBase {

  val qctx: Quotes
  import qctx.reflect._

  protected def shift: Int

  inline final protected def logStart(inline s: String): Unit = {
    if (debug) println(" " * shift + s)
  }

  inline final protected def log(inline s: String): Unit = {
    if (debug) println(" " * shift + " -> " + s)
  }

  inline final protected def logTpeAttrs[T](inline uns: TypeTree): Unit = {
    if (debug) {
      val tree = uns
      val symbol = tree.symbol
      println(s"Attrs[${tree.show}]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
    }
  }
}
