package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef.{AbstractReference, NameReference}

import java.nio.charset.StandardCharsets
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl

import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    val res = {
      val ref = TypeInspections.apply[T]
      val fullDb = TypeInspections.fullDb[T]
      val nameDb = TypeInspections.nameDb[T]
      // val ref = NameReference("xa")
      // val nameDb = Map.empty[NameReference, Set[NameReference]] // FIXME: slowness is in nameDb, fullDb is fine
      // val fullDb = Map.empty[AbstractReference, Set[AbstractReference]]
      LightTypeTag(ref, fullDb, nameDb)
    }

    val hashCodeRef = res.hashCode()
    val strRef = PickleImpl.serializeIntoString(res.ref, LightTypeTag.lttRefSerializer)
    val strDbs = PickleImpl.serializeIntoString(SubtypeDBs(res.basesdb, res.idb), LightTypeTag.subtypeDBsSerializer)

    if (valueOf[InspectorBase#debug]) {
      def string2hex(str: String): String = str.toList.map(_.toInt.toHexString).mkString
      println(s"${res.ref} => ${strRef.size} bytes, ${string2hex(strRef)}")
      println(s"${SubtypeDBs(res.basesdb, res.idb)} => ${strDbs.size} bytes, ${string2hex(strDbs)}")
      println(strDbs)
    }
    '{ LightTypeTag.parse(${Expr(hashCodeRef)}, ${Expr(strRef)}, ${Expr(strDbs)}, ${Expr(LightTypeTag.currentBinaryFormatVersion)}) }
  }
}
