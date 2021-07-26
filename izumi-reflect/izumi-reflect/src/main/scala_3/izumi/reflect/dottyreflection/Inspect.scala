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
    val ref = TypeInspections.apply[T]
    val nameDb = TypeInspections.nameDb[T]
    val fullDb = TypeInspections.fullDb[T]
//    val ref = NameReference("xa")
//    val nameDb = Map.empty[NameReference, Set[NameReference]] // FIXME: slowness is in nameDb, fullDb is fine
//    val fullDb = Map.empty[AbstractReference, Set[AbstractReference]]
    val dbs = SubtypeDBs(fullDb, nameDb)

    val strRef = PickleImpl.serializeIntoString(ref, LightTypeTag.lttRefSerializer)
    val strDbs = PickleImpl.serializeIntoString(dbs, LightTypeTag.subtypeDBsSerializer)

    def string2hex(str: String): String = {
        str.toList.map(_.toInt.toHexString).mkString
    }
    if (valueOf[InspectorBase#debug]) {
      println(s"$ref => ${strRef.size} bytes, ${string2hex(strRef)}")
      println(s"$dbs => ${strDbs.size} bytes, ${string2hex(strDbs)}")
      println(strDbs)
    }
    '{ LightTypeTag.parse(${Expr(ref.hashCode() * 31)}, ${Expr(strRef)}, ${Expr(strDbs)}, ${Expr(LightTypeTag.currentBinaryFormatVersion)}) }
  }
}
