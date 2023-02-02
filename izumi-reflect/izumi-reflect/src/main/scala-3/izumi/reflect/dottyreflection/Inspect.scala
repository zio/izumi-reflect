package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl

import java.rmi.server.LogStream.log
import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    val ltt = {
      val ref = TypeInspections.apply[T]
      val fullDb = TypeInspections.fullDb[T]
      val nameDb = TypeInspections.unappliedDb[T]
      LightTypeTag(ref, fullDb, nameDb)
    }
    makeParsedLightTypeTagImpl(ltt)
  }

  def inspectStrong[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    if (ReflectionUtil.allPartsStrong(0, Set.empty, tpe)) {
      inspectAny[T]
    } else {
      report.errorAndAbort(s"Can't materialize LTag[$tpe]: found unresolved type parameters in $tpe")
    }
  }

  def makeParsedLightTypeTagImpl(ltt: LightTypeTag)(using qctx: Quotes): Expr[LightTypeTag] = {
    val hashCodeRef = ltt.hashCode()
    val strRef = PickleImpl.serializeIntoString(ltt.ref, LightTypeTag.lttRefSerializer)
    val strDbs = PickleImpl.serializeIntoString(SubtypeDBs(ltt.basesdb, ltt.idb), LightTypeTag.subtypeDBsSerializer)

    InspectorBase.ifDebug {
      def string2hex(str: String): String = str.toList.map(_.toInt.toHexString).mkString

      println(s"${ltt.ref} => ${strRef.size} bytes, ${string2hex(strRef)}")
      println(s"${SubtypeDBs(ltt.basesdb, ltt.idb)} => ${strDbs.size} bytes, ${string2hex(strDbs)}")
      println(strDbs)
    }

    '{ LightTypeTag.parse(${ Expr(hashCodeRef) }, ${ Expr(strRef) }, ${ Expr(strDbs) }, ${ Expr(LightTypeTag.currentBinaryFormatVersion) }) }
  }

}
