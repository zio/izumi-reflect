package izumi.reflect.dottyreflection

import scala.annotation.tailrec
import scala.quoted.Quotes

trait InspectorBase extends ReflectionUtil {

  val qctx: Quotes
  import qctx.reflect._

  protected def shift: Int

  protected final lazy val ignoredSyms: Set[Symbol] = Set(defn.AnyClass, defn.AnyRefClass, defn.ObjectClass, defn.MatchableClass)
  protected final def ignored(tpe: TypeRepr): Boolean = ignoredSyms(tpe.typeSymbol)

  // FIXME reimplement TrivialMacroLogger on Scala 3
  inline val debug = false
  final type debug = debug.type

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
      println(
        s"Attrs[${tree.show}]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}"
      )
    }
  }

  extension (tpe0: TypeRepr) {

    @tailrec protected final def _fullNormDealiasSimplified: TypeRepr = {
      val tpe = tpe0.simplified
      val t = tpe._norm.dealias
      inline def tpeAnyRef = tpe.asInstanceOf[AnyRef] // workaround for `implicit conversion result must be more specific than AnyRef`
      if (t.asInstanceOf[AnyRef] eq tpeAnyRef) {
        t
      } else {
        t._fullNormDealiasSimplified
      }
    }

    @tailrec protected final def _norm: TypeRepr = {
      tpe0 match {
        case anno: AnnotatedType =>
          anno.underlying._norm
        case ref: Refinement =>
          ref.parent._norm
        case other =>
          other
      }
    }

    /**
      *  +- TypeRepr -+- NamedType -+- TermRef
      *               |             +- TypeRef
      *               +- ConstantType
      *               +- SuperType
      *               +- Refinement
      *               +- AppliedType
      *               +- AnnotatedType
      *               +- AndOrType -+- AndType
      *               |             +- OrType
      *               +- MatchType
      *               +- ByNameType
      *               +- ParamRef
      *               +- ThisType
      *               +- RecursiveThis
      *               +- RecursiveType
      *               +- LambdaType -+- MethodOrPoly -+- MethodType
      *               |              |                +- PolyType
      *               |              +- TypeLambda
      *               +- MatchCase
      *               +- TypeBounds
      *               +- NoPrefix
      */
    protected final infix def _exhaustiveMatch[A](
      f: TermRef | TypeRef | ConstantType | SuperType | Refinement | AppliedType | AnnotatedType | AndType | OrType | MatchType | ByNameType | ParamRef | ThisType |
        RecursiveThis | RecursiveType | LambdaType | MatchCase | TypeBounds | NoPrefix => A
    ): A = {
      f(tpe0.asInstanceOf)
    }

  }

}
