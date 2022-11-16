package izumi.reflect.dottyreflection

import scala.quoted.Quotes

trait InspectorBase extends ReflectionUtil {

  val qctx: Quotes
  import qctx.reflect._

  protected def shift: Int

  protected final lazy val ignoredSyms: Set[Symbol] = Set(defn.AnyClass, defn.AnyRefClass, defn.ObjectClass, defn.MatchableClass)
  protected final def ignored(tpe: TypeRepr): Boolean = ignoredSyms(tpe.typeSymbol)

  // FIXME reimplement TrivialMacroLogger on Scala 3
  inline def debug: debug = valueOf[debug]
  final type debug = false

  inline final protected def logStart(inline s: String): Unit = {
    inline if (debug) println(" " * shift + s)
  }

  inline final protected def log(inline s: String): Unit = {
    inline if (debug) println(" " * shift + " -> " + s)
  }

  inline final protected def logTpeAttrs[T](inline typeRepr: TypeRepr): Unit = {
    inline if (debug) {
      val tree = TypeTree.of(using typeRepr.asType)
      val symbol = tree.symbol
      println(
        s"Attrs[${tree.show}]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}"
      )
    }
  }

}

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
