package izumi.reflect.dottyreflection

import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef._
import reflect.Selectable.reflectiveSelectable
import java.nio.charset.StandardCharsets
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich


abstract class Inspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.tasty.{Type => TType, given _, _}
  // @formatter:on

  private def next() = new Inspector(shift + 1) {
    val qctx: self.qctx.type = self.qctx
  }

  def buildTypeRef[T <: AnyKind : Type]: AbstractReference = {
    val tpe = implicitly[Type[T]]
    val uns = tpe.unseal
    log(s" -------- about to inspect ${tpe} --------")
    val v = inspectTree(uns)
    log(s" -------- done inspecting ${tpe} --------")
    v
  }

  private[dottyreflection] def inspectTType(tpe2: TType): AbstractReference = {
    tpe2 match {
      case a: AppliedType =>
        a.args match {
          case Nil =>
            asNameRef(a.tycon)
          case o =>
            // https://github.com/lampepfl/dotty/issues/8520
            val tycontree = a.tycon.asInstanceOf[TypeRef].typeSymbol.tree.asInstanceOf[TypeDef].rhs.asInstanceOf[TypeTree]
            log(s"TYCON: ${tycontree}")
            val params: List[Option[Tree]] = try {
              val d = tycontree.asInstanceOf[{def constr: DefDef}]
              log(s"TYCONDD: ${d.constr}")
              log(d.constr.toString)
              log(d.constr.typeParams.toString)
              d.constr.typeParams.map(p => Some(p.rhs))
            } catch {
              case t: Throwable =>
                //println(s"FAILEDON: ${tycontree}")
                List.fill(a.args.size)(None)
            }

            val zargs = a.args.zip(params)

            val args = zargs.map {
              case (tpe, defn) =>
                next().inspectToB(tpe, defn)
            }
            val nameref = asNameRef(a.tycon)
            FullReference(nameref.ref.name, args, prefix = nameref.prefix)
        }

      case l: TypeLambda =>
        val resType = next().inspectTType(l.resType)
        val paramNames = l.paramNames.map {
          LambdaParameter(_)
        }
        LightTypeTagRef.Lambda(paramNames, resType)

      case t: ParamRef =>
        asNameRef(t)

      case a: AndType =>
        IntersectionReference(flattenInspectAnd(a))

      case o: OrType =>
        UnionReference(flattenInspectOr(o))

      case r: TypeRef =>
        next().inspectSymbol(r.typeSymbol)

      case tb: TypeBounds => // weird thingy
        next().inspectTType(tb.hi)

      case o =>
        log(s"TTYPE, UNSUPPORTED: $o")
        throw new RuntimeException(s"TTYPE, UNSUPPORTED: ${o.getClass} - $o")
      //???

    }
  }

  private[dottyreflection] def inspectTree(uns: TypeTree): AbstractReference = {
    val symbol = uns.symbol
    val tpe2 = uns.tpe
//    logStart(s"INSPECT: $uns: ${uns.getClass}")
    if (symbol.isNoSymbol)
      inspectTType(tpe2)
    else
      inspectSymbol(symbol)
  }

  private[dottyreflection] def inspectSymbol(symbol: Symbol): AbstractReference = {
    symbol.tree match {
      case c: ClassDef =>
        asNameRefSym(symbol)
      case t: TypeDef =>
        next().inspectTree(t.rhs.asInstanceOf[TypeTree])
      case d: DefDef =>
        next().inspectTree(d.returnTpt)
      case o =>
        log(s"SYMBOL TREE, UNSUPPORTED: $o")
        throw new RuntimeException(s"SYMBOL TREE, UNSUPPORTED: ${o.getClass} - $o")
    }
  }

  private def prefixOf(symbol: Symbol): Option[AppliedReference] = {
    if (symbol.maybeOwner.isNoSymbol) {
      None
    } else {
      symbol.maybeOwner.tree match {
        case _: PackageDef =>
          None
        case o =>
          inspectSymbol(symbol.maybeOwner) match {
            case a: AppliedReference =>
              Some(a)
            case _ =>
              None
          }
      }
    }
  }

  private def inspectToB(tpe: TypeOrBounds, td: Option[Tree]): TypeParam = {
    val variance = td match {
      case Some(value: TypeRef) =>
        extractVariance(value.typeSymbol)
      case _ =>
        Variance.Invariant
    }

    tpe match {
      case t: TypeBounds =>
        TypeParam(inspectTType(t.hi), variance)
      case t: TType =>
        TypeParam(inspectTType(t), variance)
    }
  }


  private def extractVariance(t: Symbol) = {
    if (t.flags.is(Flags.Covariant)) {
      Variance.Covariant
    } else if (t.flags.is(Flags.Contravariant)) {
      Variance.Contravariant
    } else {
      Variance.Invariant
    }
  }

  private def flattenInspectAnd(and: AndType): Set[AppliedReference] = {
    val (andTypes, otherTypes) =
      and match {
        case AndType(l@AndType(_, _), r@AndType(_, _)) =>
          (Set(l, r), Set.empty[TType])
        case AndType(l@AndType(_, _), r) =>
          (Set(l), Set(r))
        case AndType(l, r@AndType(_, _)) =>
          (Set(r), Set(l))
        case AndType(l, r) =>
          (Set.empty[AndType], Set(l, r))
      }
    val andTypeTags = andTypes flatMap flattenInspectAnd
    val otherTypeTags = otherTypes map inspectTType map {
      _.asInstanceOf[AppliedReference]
    }
    andTypeTags ++ otherTypeTags
  }

  private def flattenInspectOr(or: OrType): Set[AppliedReference] = {
    val (orTypes, otherTypes) =
      or match {
        case OrType(l@OrType(_, _), r@OrType(_, _)) =>
          (Set(l, r), Set.empty[TType])
        case OrType(l@OrType(_, _), r) =>
          (Set(l), Set(r))
        case OrType(l, r@OrType(_, _)) =>
          (Set(r), Set(l))
        case OrType(l, r) =>
          (Set.empty[OrType], Set(l, r))
      }
    val orTypeTags = orTypes flatMap flattenInspectOr
    val otherTypeTags = otherTypes map inspectTType map {
      _.asInstanceOf[AppliedReference]
    }
    orTypeTags ++ otherTypeTags
  }

  private def asNameRef(t: TType): NameReference = {
    t match {
      case ref: TypeRef =>
        asNameRefSym(ref.typeSymbol)
      case t: ParamRef =>
        NameReference(t.binder.asInstanceOf[ {def paramNames: List[Object]}].paramNames(t.paramNum).toString)
    }
  }

  private[dottyreflection] def asNameRefSym(t: Symbol): NameReference = {
    val prefix = prefixOf(t)
    NameReference(SymName.SymTypeName(t.fullName), prefix = prefix)
  }
}

