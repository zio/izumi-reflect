package izumi.reflect

import scala.quoted.*
import izumi.reflect.macros.Scala3CachingMacros

object TagGenerator {
  inline def cachedTag[T]: LightTypeTag = ${ Scala3CachingMacros.cachedTag[T] }
  inline def nonCachedTag[T]: LightTypeTag = ${ Scala3CachingMacros.computeLightTypeTag[T] }
}
