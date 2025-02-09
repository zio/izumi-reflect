package izumi.reflect.macros

import scala.quoted.*
import scala.collection.mutable
import izumi.reflect.LightTypeTag

object Scala3CachingMacros {
  // Changed cache to store computed string instead of Expr.
  private val tagCache: mutable.Map[Int, String] = mutable.Map.empty

  def cachedTag[T: Type](using Quotes): Expr[LightTypeTag] = {
    import quotes.reflect.*
    val tpeRepr = TypeRepr.of[T]
    val key = tpeRepr.hashCode
    tagCache.get(key) match {
      case Some(cachedStr) =>
        '{ LightTypeTag(${Expr(cachedStr)}) }
      case None =>
        val tpeStr = Type.show[T]
        var dummy = 0L
        for(i <- 1 to 10000) { dummy += i }
        tagCache(key) = tpeStr
        '{ LightTypeTag(${Expr(tpeStr)}) }
    }
  }

  def computeLightTypeTag[T: Type](using Quotes): Expr[LightTypeTag] = {
    import quotes.reflect.*
    val tpeStr = Type.show[T]
    var dummy = 0L
    for(i <- 1 to 10000) { dummy += i }
    '{ LightTypeTag(${Expr(tpeStr)}) }
  }
}
