package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef._

import reflect.Selectable.reflectiveSelectable
import java.nio.charset.StandardCharsets

import izumi.reflect.Tag.auto.T
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich

import scala.quoted.{Expr, Quotes, Type}

object Inspect {
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    val ref = TypeInspections.apply[T]
    val nameDb = TypeInspections.nameDb[T]
    val fullDb = TypeInspections.fullDb[T]
    val dbs = SubtypeDBs(fullDb, nameDb)

    inline def serialize[A: Pickler](a: A): String = {
      val buf = PickleImpl(a).toByteBuffer
      new String(buf.array(), buf.arrayOffset(), buf.limit(), StandardCharsets.ISO_8859_1)
    }
    val strRef = serialize(ref)(LightTypeTag.lttRefSerializer)
    val strDbs = serialize(dbs)(LightTypeTag.subtypeDBsSerializer)

    def string2hex(str: String): String = {
        str.toList.map(_.toInt.toHexString).mkString
    }
    //println(s"$ref => ${strRef.size} bytes, ${string2hex(strRef)}")
    //println(s"$dbs => ${strDbs.size} bytes, ${string2hex(strDbs)}")
    //prinltn(strDBs)
    '{ LightTypeTag.parse(${Expr(ref.hashCode())}, ${Expr(strRef)}, ${Expr(strDbs)}, 1) }
  }
}
