package izumi.reflect.dottyreflection

import scala.quoted.Quotes

/** Base of the Scala 3 inspectors: the `Quotes` context and the debug logging. */
trait InspectorBase extends ReflectionUtil {

  /** The `Quotes` of the macro expansion. */
  val qctx: Quotes
  import qctx.reflect._

  protected def shift: Int

  // FIXME reimplement TrivialMacroLogger for Scala 3
  /** The debug switch; its value is `false`. */
  inline def debug: debug = valueOf[debug]
  /** The type-level value of the debug switch: `false`. */
  final type debug = false

  // println instead of report.info because report.info eats all the subsequent report.info's after first.
  inline final protected def logStart(inline s: String): Unit = {
    inline if (debug) println(" " * shift + currentPositionStr + s)
  }

  inline final protected def log(inline s: String): Unit = {
    inline if (debug) println(" " * shift + currentPositionStr + " -> " + s)
  }

  inline final protected def logTpeAttrs[T](inline typeRepr: TypeRepr): Unit = {
    inline if (debug) {
      val tree = TypeTree.of(using typeRepr.asType)
      val symbol = tree.symbol
      System
        .err.println(
          currentPositionStr + ": " +
          s"Attrs[${tree.show}]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}"
        )
    }
  }

  private def currentPositionStr: String = {
    val pos = qctx.reflect.Position.ofMacroExpansion
    s"${pos.sourceFile.name}:${pos.endLine}"
  }

}

/** Debug helpers used by the macros. */
object InspectorBase {

  private[reflect] inline def ifDebug[A](inline f: => Unit): Unit = {
    inline if (valueOf[InspectorBase#debug]) {
      f
    }
  }

  private[reflect] inline def log(inline shift: Int, s: String): Unit = {
    inline if (valueOf[InspectorBase#debug]) println(" " * shift + " -> " + s)
  }

}
