package izumi.reflect

import scala.quoted.*

import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.dottyreflection.Inspect

object TagMacro {

  implicit inline def tag[A <: AnyKind]: Tag[A] = ${ createTagExpr[A] }

   def createTagExpr[A <: AnyKind: Type](using Quotes): Expr[Tag[A]] =
     new TagMacro().createTagExpr[A]
}

class TagMacro(using val ctx: Quotes) {
  import ctx.reflect.*

  def createTagExpr[A <: AnyKind: Type]: Expr[Tag[A]] = {
    val typeRepr = TypeRepr.of[A].dealias
    if (allPartsStrong(typeRepr))
      '{ Tag(classOf[Any], Inspect.inspect[A]) }
    else {
      val result = mkTag(typeRepr)
      result.asInstanceOf[Expr[Tag[A]]]
    }
  }

  private def mkLtt(typeRepr: TypeRepr): Expr[LightTypeTag] = 
    if (allPartsStrong(typeRepr))
      typeRepr.asType match {
        case '[a] => '{ Inspect.inspect[a] }
      }
    else {
      val result = mkTag(typeRepr)
      '{ $result.tag }
    }
  
  private def mkTag(typeRepr: TypeRepr): Expr[Tag[?]] = 
    typeRepr.dealias match {

      case x if x.typeSymbol.isTypeParam => summonTag(x)

      case AppliedType(tpe, args) =>
        val argsTags = Expr.ofList[LightTypeTag](args.map(arg => mkLtt(arg)))

        tpe.asType match {
          case '[a] => 
            val tag = '{ Tag(classOf[Any], Inspect.inspect[a]) }
            '{ Tag.appliedTag($tag, $argsTags) }
        }

      case andType: AndType => 
        val ltts0: List[Expr[LightTypeTag]] = flattenAnd(andType).map(tt => mkLtt(tt))
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(ltts0)
        andType.asType match {
          case '[a] => 
            val struct = '{ Inspect.inspect[a] }
            '{ Tag.refinedTag(classOf[Any], $ltts, $struct, Map.empty)}    
        }

      // TODO: This ain't right
      case orType: OrType => 
        val ltts0: List[Expr[LightTypeTag]] = flattenOr(orType).map(tt => mkLtt(tt))
        val ltts: Expr[List[LightTypeTag]] = Expr.ofList(ltts0)
        orType.asType match {
          case '[a] => 
            val struct = '{ Inspect.inspect[a] }
            '{ Tag.refinedTag(classOf[Any], $ltts, $struct, Map.empty)}
        }

      case _ => 
          throw new Exception(s"Unsupported type: $typeRepr")
    }

  private def flattenAnd(tpe: TypeRepr): List[TypeRepr] =   
    tpe match {
      case AndType(lhs, rhs) => flattenAnd(lhs) ++ flattenAnd(rhs)
      case _ => List(tpe)
    }

  private def flattenOr(tpe: TypeRepr): List[TypeRepr] =   
    tpe match {
      case OrType(lhs, rhs) => flattenOr(lhs) ++ flattenOr(rhs)
      case _ => List(tpe)
    }


  private def summonTag(typeRepr: TypeRepr): Expr[Tag[?]] = {
    typeRepr.asType match {
      case '[a] => 
        val message = s"Cannot find implicit Tag[${Type.show[a]}]"
        Expr.summon[Tag[a]].getOrElse(report.errorAndAbort(message))
    }
  }

  /**
   * Returns true if the given type contains no type parameters 
   * (this means the type is not "weak" https://stackoverflow.com/questions/29435985/weaktypetag-v-typetag)
   */ 
  private def allPartsStrong(typeRepr: TypeRepr): Boolean = 
      typeRepr match {
        case x if x.typeSymbol.isTypeParam => false
        case AppliedType(tpe, args) => allPartsStrong(tpe) && args.forall(allPartsStrong)
        case AndType(lhs, rhs) => allPartsStrong(lhs) && allPartsStrong(rhs)
        case OrType(lhs, rhs) => allPartsStrong(lhs) && allPartsStrong(rhs)
        case TypeRef(tpe, name) => allPartsStrong(tpe) 
        case TermRef(tpe, name) => allPartsStrong(tpe) 
        case ThisType(tpe) => allPartsStrong(tpe)
        case NoPrefix() => true
        case TypeBounds(lo, hi) => allPartsStrong(lo) && allPartsStrong(hi)
        case TypeLambda(_, _, body) => allPartsStrong(body)
        case _ : TypeParamClause => true
      }
  
}
