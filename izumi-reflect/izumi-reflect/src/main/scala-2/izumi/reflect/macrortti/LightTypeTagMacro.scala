/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.reflect.macrortti

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.{DebugProperties, ReflectionUtil, TrivialMacroLogger}

import scala.reflect.macros.blackbox

final class LightTypeTagMacro(override val c: blackbox.Context)
  extends LightTypeTagMacro0[blackbox.Context](c)(
    logger = TrivialMacroLogger.make[LightTypeTagMacro](c)
  )

private[reflect] class LightTypeTagMacro0[C <: blackbox.Context](val c: C)(logger: TrivialLogger) {

  import c.universe._

  protected final def cacheEnabled: Boolean = !c.settings.contains(s"${DebugProperties.`izumi.reflect.rtti.cache.compile`}=false")
  protected final val impl = new LightTypeTagImpl[c.universe.type](c.universe, withCache = cacheEnabled, logger)

  final def makeStrongHKTag[ArgStruct: c.WeakTypeTag]: c.Expr[LTag.StrongHK[ArgStruct]] = {
    val tpe = unpackArgStruct(weakTypeOf[ArgStruct])
    if (ReflectionUtil.allPartsStrong(tpe)) {
      c.Expr[LTag.StrongHK[ArgStruct]](q"new ${weakTypeOf[LTag.StrongHK[ArgStruct]]}(${makeParsedLightTypeTagImpl(tpe)})")
    } else {
      c.abort(c.enclosingPosition, s"Can't materialize LTag.StrongHKTag[$tpe]: found unresolved type parameters in $tpe")
    }
  }

  final def makeWeakHKTag[ArgStruct: c.WeakTypeTag]: c.Expr[LTag.WeakHK[ArgStruct]] = {
    c.Expr[LTag.WeakHK[ArgStruct]](q"new ${weakTypeOf[LTag.WeakHK[ArgStruct]]}(${makeParsedLightTypeTagImpl(unpackArgStruct(weakTypeOf[ArgStruct]))})")
  }

  final def makeStrongTag[T: c.WeakTypeTag]: c.Expr[LTag[T]] = {
    val tpe = weakTypeOf[T]
    if (ReflectionUtil.allPartsStrong(tpe.dealias)) {
      val res = makeParsedLightTypeTagImpl(tpe)
      c.Expr[LTag[T]](q"new ${weakTypeOf[LTag[T]]}($res)")
    } else {
      c.abort(c.enclosingPosition, s"Can't materialize LTag[$tpe]: found unresolved type parameters in $tpe")
    }
  }

  final def makeWeakTag[T: c.WeakTypeTag]: c.Expr[LTag.Weak[T]] = {
    val res = makeParsedLightTypeTagImpl(weakTypeOf[T])
    c.Expr[LTag.Weak[T]](q"new ${weakTypeOf[LTag.Weak[T]]}($res)")
  }

  final def makeParsedLightTypeTag[T: c.WeakTypeTag]: c.Expr[LightTypeTag] = {
    makeParsedLightTypeTagImpl(weakTypeOf[T])
  }

  final def makeParsedLightTypeTagImpl(tpe: Type): c.Expr[LightTypeTag] = {
    val res = impl.makeFullTagImpl(tpe)
    makeParsedLightTypeTagImpl(res)
  }

  final def makeParsedLightTypeTagImpl(ltt: LightTypeTag): c.Expr[LightTypeTag] = {
    logger.log(s"LightTypeTagImpl: created LightTypeTag: $ltt")

    val hashCodeRef = ltt.hashCode()
    val strRef = PickleImpl.serializeIntoString(ltt.ref, LightTypeTag.lttRefSerializer)
    val strDBs = PickleImpl.serializeIntoString(SubtypeDBs(ltt.basesdb, ltt.idb), LightTypeTag.subtypeDBsSerializer)

    c.Expr[LightTypeTag](
      q"_root_.izumi.reflect.macrortti.LightTypeTag.parse($hashCodeRef: _root_.scala.Int, $strRef : _root_.java.lang.String, $strDBs : _root_.java.lang.String, ${LightTypeTag.currentBinaryFormatVersion}: _root_.scala.Int)"
    )
  }

  @inline final def unpackArgStruct(t: Type): Type = {
    def badShapeError() = {
      c.abort(
        c.enclosingPosition,
        s"Expected type shape RefinedType `{ type Arg[A] = X[A] }` for summoning `LTag.StrongHK/WeakHK[X]`, but got $t (raw: ${showRaw(t)} ${t.getClass})"
      )
    }
    t match {
      case r: RefinedTypeApi =>
        r.decl(TypeName("Arg")) match {
          case sym: TypeSymbolApi =>
            sym.info.typeConstructor
          case _ => badShapeError()
        }
      case _ => badShapeError()
    }
  }

}
