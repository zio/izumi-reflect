package izumi.reflect.dottyreflection

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl

import scala.quoted.{Expr, Quotes, Type}

/** Macro entry points that build the `LightTypeTag` of a type. */
object Inspect {
  /** Returns the `LightTypeTag` of `T`. */
  inline def inspect[T <: AnyKind]: LightTypeTag = ${ inspectAny[T] }

  /** Like `inspect`, but fails the compilation when `T` has unresolved type parameters. */
  inline def inspectStrong[T <: AnyKind]: LightTypeTag = ${ inspectStrong[T] }

  /** Returns the expression of the `LightTypeTag` of `T`. */
  def inspectAny[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    inspectTypeRepr(qctx.reflect.TypeRepr.of[T])
  }

  /** Returns the expression of the `LightTypeTag` of `typeRepr`: its reference and its two databases. */
  def inspectTypeRepr(using qctx: Quotes)(typeRepr: qctx.reflect.TypeRepr): Expr[LightTypeTag] = {
    val ltt = {
      val ref = TypeInspections(typeRepr)
      val fullDb = TypeInspections.fullDb(typeRepr)
      val nameDb = TypeInspections.unappliedDb(typeRepr)
      LightTypeTag(ref, fullDb, nameDb)
    }
    makeParsedLightTypeTagImpl(ltt)
  }

  /** Returns the expression of the `LightTypeTag` of `T`; aborts compilation when `T` has unresolved type parameters. */
  def inspectStrong[T <: AnyKind: Type](using qctx: Quotes): Expr[LightTypeTag] = {
    import qctx.reflect.*
    val tpe = TypeRepr.of[T]
    val owners = ReflectionUtil.getClassDefOwners(Symbol.spliceOwner)
    if (ReflectionUtil.allPartsStrong(0, owners, Set.empty, tpe)) {
      inspectAny[T]
    } else {
      report.errorAndAbort(s"Can't materialize LTag[$tpe]: found unresolved type parameters in $tpe")
    }
  }

  /** Returns an expression that parses the serialized form of `ltt` with `LightTypeTag.parse`. */
  def makeParsedLightTypeTagImpl(ltt: LightTypeTag)(using qctx: Quotes): Expr[LightTypeTag] = {
    val serialized = ltt.serialize()
    val hashCodeRef = serialized.hash
    val strRef = serialized.ref
    val strDBs = serialized.databases

    InspectorBase.ifDebug {
      def string2hex(str: String): String = str.toList.map(_.toInt.toHexString).mkString

      println(s"${ltt.ref} => ${strRef.size} bytes, ${string2hex(strRef)}")
      println(s"${SubtypeDBs.make(ltt.basesdb, ltt.idb)} => ${strDBs.size} bytes, ${string2hex(strDBs)}")
      println(strDBs)
    }

    '{ LightTypeTag.parse(${ Expr(hashCodeRef) }, ${ Expr(strRef) }, ${ Expr(strDBs) }, ${ Expr(LightTypeTag.currentBinaryFormatVersion) }) }
  }

}
