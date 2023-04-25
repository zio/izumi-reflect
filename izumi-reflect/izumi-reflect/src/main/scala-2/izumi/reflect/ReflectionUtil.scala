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

package izumi.reflect

import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger

import scala.annotation.tailrec
import scala.reflect.api.Universe

private[reflect] object ReflectionUtil {

  /** Mini `normalize`. `normalize` is deprecated and we don't want to do scary things such as evaluate type-lambdas anyway.
    * And AFAIK the only case that can make us confuse a type-parameter for a non-parameter is an empty refinement `T {}`.
    * So we just strip it when we get it.
    */
  // ReflectionUtil.norm but with added logging
  @tailrec @inline
  def norm(u: Universe, logger: TrivialLogger)(x: u.Type): u.Type = {
    import u._
    x match {
      case RefinedType(t :: Nil, m) if m.isEmpty =>
        logger.log(s"Stripped empty refinement of type $t. member scope $m")
        norm(u, logger)(t)
      case AnnotatedType(_, t) =>
        norm(u, logger)(t)
      case _ =>
        x
    }
  }

  def allPartsStrong(tpe: Universe#Type): Boolean = {
    allPartsStrong(Set.empty, tpe)
  }

  def allPartsStrong(outerTypeParams: Set[Universe#Symbol], tpeRaw: Universe#Type): Boolean = {
    val dealiased = tpeRaw.dealias
    val selfStrong = isSelfStrong(outerTypeParams, dealiased) || outerTypeParams.contains(dealiased.typeSymbol)

    dealiased match {
      case t: Universe#RefinedTypeApi =>
        t.parents.forall(allPartsStrong(outerTypeParams, _)) &&
        t.decls.toSeq.forall((s: Universe#Symbol) => s.isTerm || allPartsStrong(outerTypeParams, s.asType.typeSignature.dealias))
      case _ =>
        val resType = dealiased.finalResultType
        if (dealiased.takesTypeArgs && !dealiased.typeParams.forall(outerTypeParams.contains)) {
          allPartsStrong(outerTypeParams ++ dealiased.typeParams, resType)
        } else {

          def typeCtorStrong: Boolean = {
            val ctor = resType.typeConstructor
            (resType == dealiased) || (ctor == dealiased) || (ctor == tpeRaw) ||
            outerTypeParams.contains(ctor.typeSymbol) || allPartsStrong(outerTypeParams, ctor)
          }

          def argsStrong: Boolean = {
            resType.typeArgs.forall {
              arg =>
                outerTypeParams.contains(arg.typeSymbol) || allPartsStrong(outerTypeParams, arg)
            }
          }

          selfStrong && /*prefixStrong &&*/ typeCtorStrong && argsStrong
        }
    }
  }

  def isSelfStrong(outerTypeParams: Set[Universe#Symbol], tpe: Universe#Type): Boolean = {
    // FIXME: strengthening check to capture `IntersectionBlockingIO` test case causes StackOverflow during implicit search
//    def intersectionMembersStrong = {
//      tpe match {
//        case t: Universe#RefinedTypeApi =>
//          t.parents.forall(isSelfStrong)
//        case _ => true
//      }
//    }

    def prefixStrong: Boolean = {
      tpe match {
        case t: Universe#TypeRefApi =>
          allPartsStrong(outerTypeParams, t.pre.dealias)
        case _ =>
          true
      }
    }

    (prefixStrong && !(tpe.typeSymbol.isParameter || (
      // we regard abstract types like T in trait X { type T; Tag[this.T] } - when we are _inside_ the definition template
      // as 'type parameters' too. So that you could define `implicit def tagForT: Tag[this.T]` and the tag would be resolved
      // to this implicit correctly, instead of generating a useless `X::this.type::T` tag.
      tpe.isInstanceOf[Universe#TypeRefApi] &&
      tpe.asInstanceOf[Universe#TypeRefApi].pre.isInstanceOf[Universe#ThisTypeApi] &&
      tpe.typeSymbol.isAbstract && !tpe.typeSymbol.isClass && isNotDealiasedFurther(tpe)
    ))) /*&& intersectionMembersStrong*/ ||
    tpe.typeParams.exists { // is identity
      t =>
        t == tpe.typeSymbol ||
        t.typeSignature == tpe.typeSymbol.typeSignature ||
        (t.name eq tpe.typeSymbol.name)
    }
  }

  def isNotDealiasedFurther(tpe: Universe#Type): Boolean = {
    val u: Universe = null
    val tpe1: u.Type = tpe.asInstanceOf[u.Type]
    tpe1.dealias =:= tpe1
  }

  def kindOf(tpe: Universe#Type): Kind = {
    Kind(tpe.typeParams.map(t => kindOf(t.typeSignature)))
  }

  final case class Kind(args: List[Kind]) {
    def format(typeName: String) = s"$typeName${if (args.nonEmpty) args.mkString("[", ", ", "]") else ""}"
    override def toString: String = format("_")
  }

}
