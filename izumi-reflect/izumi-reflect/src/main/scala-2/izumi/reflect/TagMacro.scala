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

import scala.annotation.nowarn
import izumi.reflect.internal.fundamentals.platform.console.TrivialLogger
import izumi.reflect.ReflectionUtil.{Kind, kindOf}
import izumi.reflect.TagMacro._
import izumi.reflect.macrortti.LightTypeTagRef.{FullReference, LambdaParameter, NameReference, SymName, TypeParam, Variance}
import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagMacro0, LightTypeTagRef}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.api.Universe
import scala.reflect.macros.{TypecheckException, blackbox, whitebox}

// TODO: benchmark difference between searching all arguments vs. merge strategy
// TODO: benchmark ProviderMagnet vs. identity macro vs. normal function
@nowarn("msg=deprecated")
class TagMacro(val c: blackbox.Context) {

  import c.universe._

  protected[this] val logger: TrivialLogger = TrivialMacroLogger.make[this.type](c)
  private[this] val ltagMacro = new LightTypeTagMacro0[c.type](c)(logger)

  // workaround for a scalac bug - `Nothing` type is lost when two implicits for it are summoned from one implicit as in:
  //  implicit final def tagFromTypeTag[T](implicit t: TypeTag[T], l: LTag[T]): Tag[T] = Tag(t, l.fullLightTypeTag)
  // https://github.com/scala/bug/issues/11715
  final def makeTag[T: c.WeakTypeTag]: c.Expr[Tag[T]] = {
    val tpe = weakTypeOf[T]
    if (ReflectionUtil.allPartsStrong(tpe.dealias)) {
      logger.log(s"Got strong tag, generating LTT right away: ${weakTypeOf[T]}")
      val ltag = ltagMacro.makeParsedLightTypeTagImpl(tpe)
      val cls = closestClass(tpe)
      c.Expr[Tag[T]] {
        q"_root_.izumi.reflect.Tag.apply[$tpe]($cls, $ltag)"
      }
    } else {
      makeTagImpl[T]
    }
  }

  @inline final def makeHKTagMaterializer[ArgStruct: c.WeakTypeTag]: c.Expr[HKTagMaterializer[ArgStruct]] = {
    c.Expr[HKTagMaterializer[ArgStruct]](q"new ${weakTypeOf[HKTagMaterializer[ArgStruct]]}(${makeHKTag[ArgStruct]})")
  }

  @inline final def makeHKTag[ArgStruct: c.WeakTypeTag]: c.Expr[HKTag[ArgStruct]] = {
    val argStruct = weakTypeOf[ArgStruct]
    val ctor = ltagMacro.unpackArgStruct(argStruct)
    if (ReflectionUtil.allPartsStrong(ctor)) {
      logger.log(s"HK: found Strong ctor=$ctor in ArgStruct, returning $argStruct")
      makeHKTagFromStrongTpe(ctor)
    } else {
      makeHKTagImpl(ctor)
    }
  }

  // FIXME: nearly a copypaste of mkTagWithTypeParameters, deduplicate?
  private[this] def makeHKTagImpl[ArgStruct: c.WeakTypeTag](outerLambda: Type): c.Expr[HKTag[ArgStruct]] = {
    logger.log(s"Got unresolved HKTag summon: ${tagFormat(outerLambda)} from ArgStruct: ${weakTypeOf[ArgStruct]}")

    def isLambdaParamOf(arg: Type, lam: Type): Boolean = {
      lam.typeParams.contains(arg.typeSymbol)
    }

    val lambdaResult = outerLambda.finalResultType
    val ctorTpe = lambdaResult.typeConstructor
    val typeArgsTpes = lambdaResult.typeArgs

    val isSimplePartialApplication = {
      // all parameters are consumed exactly once, in left-to-right order
      typeArgsTpes
        .map(_.typeSymbol)
        .filter(outerLambda.typeParams.contains) == outerLambda.typeParams
    }

    val constructorTag: c.Expr[HKTag[_]] = {
      getCtorKindIfCtorIsTypeParameter(ctorTpe) match {
        // type constructor of this type is not a type parameter
        // BUT can be an intersection type
        // some of its arguments are type parameters that we should resolve
        case None =>
          logger.log(s"HK type A ctor=$ctorTpe sym=${ctorTpe.typeSymbol}")
          makeHKTagFromStrongTpe(ctorTpe)

        // error: the entire type is just a proper type parameter with no type arguments
        // it cannot be resolved further
        case Some(k) if k == kindOf(outerLambda) && isSimplePartialApplication =>
          logger.log(s"HK type B $ctorTpe ${ctorTpe.typeSymbol}")
          val msg = s"  could not find implicit value for ${tagFormat(lambdaResult)}: $lambdaResult is a type parameter without an implicit Tag!"
          addImplicitError(msg)
          abortWithImplicitError()

        // type constructor is a type parameter AND has type arguments
        // we should resolve type constructor separately from an HKTag
        case Some(hktKind) =>
          logger.log(s"HK type C $ctorTpe ${ctorTpe.typeSymbol}")
          summonHKTag(ctorTpe, hktKind)
      }
    }

    val res = {
      if (isSimplePartialApplication) {
        val embeddedMaybeNonParamTypeArgs = typeArgsTpes.map {
          arg => if (!isLambdaParamOf(arg, outerLambda)) Some(arg) else None
        }
        val argTags = {
          // FIXME: create LTT in-place instead of summoning Tag if typeArg is Strong
          logger.log(s"HK Now summoning tags for args=$embeddedMaybeNonParamTypeArgs")
          val argExprs = embeddedMaybeNonParamTypeArgs.map(_.map {
            t =>
              val dealiased = ReflectionUtil.norm(c.universe: c.universe.type, logger)(t.dealias)
              summonLightTypeTagOfAppropriateKind(dealiased)
          })
          c.Expr[List[Option[LightTypeTag]]](q"$argExprs")
        }

        reify {
          HKTag.appliedTagNonPos[ArgStruct](constructorTag.splice, argTags.splice)
        }
      } else {
        // warn if constructor is an intersection type, as they don't work properly in this position right now
        lambdaResult match {
          case t: RefinedTypeApi if t.parents.size > 1 =>
            // info instead of warning because warnings are suppressed by Scala 2 inside implicit macros for some reason
            c.info(
              c.enclosingPosition,
              s"""TODO: Pathological intersection refinement result in lambda being reconstructed result=`$lambdaResult` in the rhs of type lambda lam=`$outerLambda`
                 |Only simple applied types of form F[A] are supported in results of type lambdas. The generated tag may not work correctly.""".stripMargin,
              force = true
            )
          case _ =>
        }

        val PolyType(outerLambdaParamArgsSyms, _) = outerLambda: @unchecked

        val distinctNonParamArgsTypes = typeArgsTpes.filter(!isLambdaParamOf(_, outerLambda)).distinct

        // we give a distinct lambda parameter to the constructor, even if constructor is one of the type parameters
        val ctorLambdaParameter = LambdaParameter(SymName.scala2FirstLambdaParamName.name)

        val typeArgToLambdaParameterMap: Map[Either[Type, Symbol], LambdaParameter] =
          // for non-lambda arguments the types are unique, but symbols are not, for lambda arguments the symbols are unique but types are not.
          // it's very confusing.
          (distinctNonParamArgsTypes.map(Left(_)) ++ outerLambdaParamArgsSyms.map(Right(_)))
            .distinct.iterator.zipWithIndex.map {
              case (argTpeOrSym, idx) =>
                val idxPlusOne = idx + 1
                val lambdaParameter = LambdaParameter(s"$idxPlusOne")
                argTpeOrSym -> lambdaParameter
            }.toMap

        def getFromMap(k1: Either[Type, Symbol], k2: Either[Type, Symbol]): LambdaParameter = {
          typeArgToLambdaParameterMap.getOrElse(
            k1,
            typeArgToLambdaParameterMap.getOrElse(
              k2, {
                val msg = s"Problem: couldn't get a lambda parameter idx for k1=$k1 k2=$k2 in $typeArgToLambdaParameterMap"
                logger.log(msg)
                c.abort(c.enclosingPosition, msg)
              }
            )
          )
        }

        val usageOrderDistinctNonLambdaArgs = distinctNonParamArgsTypes.map(t => getFromMap(Left(t), Right(t.typeSymbol)))
        val declarationOrderLambdaParamArgs = outerLambdaParamArgsSyms.map(sym => getFromMap(Right(sym), Left(sym.typeSignature)))

        val usages = typeArgsTpes.map(t => TypeParam(NameReference(SymName.LambdaParamName(getFromMap(Left(t), Right(t.typeSymbol)).name)), Variance.Invariant))

        val ctorApplyingLambda = LightTypeTagRef.Lambda(
          ctorLambdaParameter :: usageOrderDistinctNonLambdaArgs ::: declarationOrderLambdaParamArgs,
          FullReference(SymName.LambdaParamName(ctorLambdaParameter.name), usages)
        )

        logger.log(s"""HK non-trivial lambda construction:
                      |ctorApplyingLambda=$ctorApplyingLambda
                      |usageOrderNonLambdaArgs=$usageOrderDistinctNonLambdaArgs
                      |declarationOrderLambdaParamArgs=$declarationOrderLambdaParamArgs
                      |""".stripMargin)

        val argTagsExceptCtor = {
          val nonParamArgsDealiased = distinctNonParamArgsTypes.map(t => ReflectionUtil.norm(c.universe: c.universe.type, logger)(t.dealias))
          logger.log(s"HK COMPLEX Now summoning tags for args=$nonParamArgsDealiased outerLambdaParams=$outerLambdaParamArgsSyms")

          c.Expr[List[Option[LightTypeTag]]] {
            q"${nonParamArgsDealiased.map(t => Some(summonLightTypeTagOfAppropriateKind(t))) ++ outerLambdaParamArgsSyms.map(_ => None)}"
          }
        }

        val outerLambdaReprTag = ltagMacro.makeParsedLightTypeTagImpl(LightTypeTag(ctorApplyingLambda, Map.empty, Map.empty))
        reify {
          val ctorTag = constructorTag.splice
          HKTag.appliedTagNonPosAux[ArgStruct](ctorTag.closestClass, outerLambdaReprTag.splice, Some(ctorTag.tag) :: argTagsExceptCtor.splice)
        }
      }
    }

    logger.log(s"Final code of ${tagFormat(outerLambda)}:\n ${showCode(res.tree)}")

    res
  }

  private[this] def makeHKTagFromStrongTpe[ArgStruct](strongCtorType: Type): c.Expr[HKTag[ArgStruct]] = {
    val ltag = ltagMacro.makeParsedLightTypeTagImpl(strongCtorType)
    val cls = closestClass(strongCtorType)
    c.Expr[HKTag[ArgStruct]] {
      q"_root_.izumi.reflect.HKTag.apply($cls, $ltag)"
    }
  }

  def makeTagImpl[T: c.WeakTypeTag]: c.Expr[Tag[T]] = {
    logger.log(s"Got non-strong tag: ${weakTypeOf[T]}")

    if (getImplicitError().endsWith(":")) { // yep
      logger.log(s"Got continuation implicit error: ${getImplicitError()}")
    } else {
      resetImplicitError(weakTypeOf[T])
      addImplicitError("\n\n<trace>: ")
    }

    val tgt = ReflectionUtil.norm(c.universe: c.universe.type, logger)(weakTypeOf[T].dealias)

    addImplicitError(s"  deriving Tag for ${weakTypeOf[T]}, dealiased: $tgt:")

    val res = tgt match {
      case RefinedType(intersection, _) =>
        mkRefined[T](intersection, tgt)
      case _ =>
        mkTagWithTypeParameters[T](tgt)
    }

    addImplicitError(s"  succeeded for: $tgt")

    logger.log(s"Final code of Tag[${weakTypeOf[T]}]:\n ${showCode(res.tree)}")

    res
  }

  @inline
  protected[this] def mkRefined[T: c.WeakTypeTag](intersection: List[Type], originalRefinement: Type): c.Expr[Tag[T]] = {
    val summonedIntersectionTags = intersection.map {
      t0 =>
        val t = ReflectionUtil.norm(c.universe: c.universe.type, logger)(t0.dealias)
        summonLightTypeTagOfAppropriateKind(t)
    }
    val intersectionTags = c.Expr[List[LightTypeTag]](Liftable.liftList[c.Expr[LightTypeTag]].apply(summonedIntersectionTags))
    val (structTag, additionalTypeMembers) = mkStruct(intersection, originalRefinement)
    val cls = closestClass(originalRefinement)

    reify {
      Tag.refinedTag[T](cls.splice, intersectionTags.splice, structTag.splice, additionalTypeMembers.splice)
    }
  }

  @inline
  protected[this] def mkStruct(intersection: List[Type], originalRefinement: Type): (c.Expr[LightTypeTag], c.Expr[Map[String, LightTypeTag]]) = {
    val (strongDecls, weakDecls) = originalRefinement
      .decls
      .partition {
        symbol =>
          // skip resolution for types in methods/vals (that would need a new runtime constructor, `methodTag`, like `refinedTag` for the case & dealing with method type parameters may be non-trivial)
          // see: "progression test: can't handle parameters in defs/vals in structural types"
          symbol.isTerm ||
          ReflectionUtil.isSelfStrong(symbol.info)
      }

    val resolvedTags = weakDecls.map {
      symbol =>
        symbol.name.decodedName.toString -> summonLightTypeTagOfAppropriateKind(symbol.info)
    }.toMap

    val strongDeclsTpe = internal.refinedType(intersection, originalRefinement.typeSymbol.owner, internal.newScopeWith(strongDecls.toSeq: _*))
    val resolvedTagsExpr = c.Expr[Map[String, LightTypeTag]](Liftable.liftMap[String, Expr[LightTypeTag]].apply(resolvedTags))

    (ltagMacro.makeParsedLightTypeTagImpl(strongDeclsTpe), resolvedTagsExpr)
  }

  // we need to handle three cases – type args, refined types and type bounds (we don't handle type bounds currently)
  @inline
  protected[this] def mkTagWithTypeParameters[T: c.WeakTypeTag](tpe: Type): c.Expr[Tag[T]] = {
    val constructorTag: c.Expr[HKTag[_]] = {
      val ctor = tpe.typeConstructor
      getCtorKindIfCtorIsTypeParameter(ctor) match {
        // type constructor of this type is not a type parameter
        // AND not an intersection type
        // some of its arguments are type parameters that we should resolve
        case None =>
          logger.log(s"type A $ctor  ${ctor.typeSymbol}")
          makeHKTagFromStrongTpe(ctor)

        // error: the entire type is just a proper type parameter with no type arguments
        // it cannot be resolved further
        case Some(Kind(Nil)) =>
          logger.log(s"type B $ctor ${ctor.typeSymbol}")
          val msg = s"  could not find implicit value for ${tagFormat(tpe)}: $tpe is a type parameter without an implicit Tag!"
          addImplicitError(msg)
          abortWithImplicitError()

        // type constructor is a type parameter AND has type arguments
        // we should resolve type constructor separately from an HKTag
        case Some(hktKind) =>
          logger.log(s"type C $ctor ${ctor.typeSymbol}")
          summonHKTag(ctor, hktKind)
      }
    }
    val argTags = {
      val args = tpe.typeArgs.map(t => ReflectionUtil.norm(c.universe: c.universe.type, logger)(t.dealias))
      logger.log(s"Now summoning tags for args=$args")
      c.Expr[List[LightTypeTag]](Liftable.liftList[c.Expr[LightTypeTag]].apply(args.map(summonLightTypeTagOfAppropriateKind)))
    }

    reify {
      Tag.appliedTag[T](constructorTag.splice, argTags.splice)
    }
  }

  @inline
  private[this] final def closestClass(properTypeStrongCtor: Type): c.Expr[Class[_]] = {
    // unfortunately .erasure returns trash for intersection types
    val tpeLub = properTypeStrongCtor match {
      case r: RefinedTypeApi => lub(r.parents)
      case _ => properTypeStrongCtor
    }
    val tpeErased = tpeLub.erasure
    // and for Scala varargs (Scala by names and Java varargs are fine)
    val tpeFixed = if (tpeErased.typeSymbol eq definitions.RepeatedParamClass) {
      typeOf[scala.Seq[Any]].dealias
    } else {
      tpeErased
    }
    c.Expr[Class[_]](q"_root_.scala.Predef.classOf[$tpeFixed]")
  }

  @inline
  private[this] final def getCtorKindIfCtorIsTypeParameter(tpe: Type): Option[Kind] = {
    if (!ReflectionUtil.isSelfStrong(tpe)) Some(kindOf(tpe))
    else None
  }

  protected[this] def mkTypeParameter(owner: Symbol, kind: Kind): Symbol = {
    import internal.reificationSupport._
    import internal.{polyType, typeBounds}

    val tpeSymbol = newNestedSymbol(owner, freshTypeName(""), NoPosition, Flag.PARAM | Flag.DEFERRED, isClass = false)

    val tpeTpe = if (kind.args.nonEmpty) {
      val params = kind.args.map(mkTypeParameter(tpeSymbol, _))

      polyType(params, typeBounds(definitions.NothingTpe, definitions.AnyTpe))
    } else {
      typeBounds(definitions.NothingTpe, definitions.AnyTpe)
    }

    setInfo(tpeSymbol, tpeTpe)

    tpeSymbol
  }

  @inline
  protected[this] def mkHKTagArgStruct(tpe: Type, kind: Kind): Type = {
    import internal.reificationSupport._

    val staticOwner = c.prefix.tree.symbol.owner

    logger.log(s"staticOwner: $staticOwner")

    val parents = List(definitions.AnyRefTpe)
    val mutRefinementSymbol: Symbol = newNestedSymbol(staticOwner, TypeName("<refinement>"), NoPosition, FlagsRepr(0L), isClass = true)

    val mutArg: Symbol = newNestedSymbol(mutRefinementSymbol, TypeName("Arg"), NoPosition, FlagsRepr(0L), isClass = false)
    val params = kind.args.map(mkTypeParameter(mutArg, _))
    setInfo(mutArg, mkPolyType(tpe, params))

    val scope = newScopeWith(mutArg)

    setInfo[Symbol](mutRefinementSymbol, RefinedType(parents, scope, mutRefinementSymbol))

    RefinedType(parents, scope, mutRefinementSymbol)
  }

  @inline
  protected[this] def mkPolyType(tpe: Type, params: List[c.Symbol]): Type = {
    val rhsParams = params.map(symbol => internal.typeRef(NoPrefix, symbol, Nil))

    internal.polyType(params, appliedType(tpe, rhsParams))
  }

  private[this] def summonLightTypeTagOfAppropriateKind(tpe: Type): c.Expr[LightTypeTag] = {
    lttFromTag(summonTagForKind(tpe, kindOf(tpe)))
  }

  private[this] def summonHKTag(tpe: Type, kind: Kind): c.Expr[HKTag[_]] = {
    c.Expr[HKTag[_]](summonTagForKind(tpe, kind))
  }

  @inline
  protected[this] def summonTagForKind(tpe: c.Type, kind: Kind): c.Tree = {
    try {
      if (kind == Kind(Nil)) {
        c.inferImplicitValue(appliedType(weakTypeOf[Tag[Nothing]].typeConstructor, tpe), silent = false)
      } else {
        val ArgStruct = mkHKTagArgStruct(tpe, kind)
        logger.log(s"Created implicit Arg: $ArgStruct")
        c.inferImplicitValue(appliedType(weakTypeOf[HKTag[Nothing]].typeConstructor, ArgStruct), silent = false)
      }
    } catch {
      case _: TypecheckException =>
        val error = hktagSummonHelpfulErrorMessage(tpe, kind)
        val msg = s"  could not find implicit value for ${tagFormat(tpe)}$error"
        addImplicitError(msg)
        abortWithImplicitError()
    }
  }

  @inline
  private[this] final def lttFromTag(tagTree: Tree): c.Expr[LightTypeTag] = {
    c.Expr[LightTypeTag](q"$tagTree.tag")
  }

  def getImplicitError(): String = {
    val annotations = symbolOf[Tag[Any]].annotations
    annotations
      .headOption.flatMap(
        AnnotationTools.findArgument(_) {
          case Literal(Constant(s: String)) => s
        }
      ).getOrElse(defaultTagImplicitError)
  }

  def abortWithImplicitError(): Nothing = {
    c.abort(c.enclosingPosition, getImplicitError())
  }

  @inline
  protected[this] def addImplicitError(err: String): Unit = {
    setImplicitError(s"${getImplicitError()}\n$err")
  }

  @inline
  protected[this] def resetImplicitError(tpe: Type): Unit = {
    setImplicitError(defaultTagImplicitError.replace("${T}", tpe.toString))
  }

  @inline
  protected[this] def setImplicitError(err: String): Unit = {
    import internal.decorators._

    symbolOf[Tag[Any]].setAnnotations(
      Annotation(typeOf[implicitNotFound], List[Tree](Literal(Constant(err))), ListMap.empty)
    )
    ()
  }

  @inline
  private[this] final def hktagSummonHelpfulErrorMessage(tpe: Type, kind: Kind): String = {
    tagFormatMap.get(kind) match {
      case Some(_) => ""
      case None =>
        val (typaramsWithKinds, appliedParams) = kind
          .args.zipWithIndex.map {
            case (k, i) =>
              val name = s"T${i + 1}"
              k.format(name) -> name
          }.unzip
        s"""
           |$tpe is of a kind $kind, which doesn't have a tag name. Please create a tag synonym as follows:
           |
           |  type TagXYZ[${kind.format(typeName = "K")}] = HKTag[ { type Arg[${typaramsWithKinds.mkString(", ")}] = K[${appliedParams.mkString(", ")}] } ]
           |
           |And use it in your context bound, as in def x[$tpe: TagXYZ] = ...
           |OR use Tag.auto.T macro, as in def x[$tpe: Tag.auto.T] = ...""".stripMargin
    }
  }

}

private object TagMacro {
  final val defaultTagImplicitError =
    "could not find implicit value for izumi.reflect.Tag[${T}]. Did you forget to put on a Tag, TagK or TagKK context bound on one of the parameters in ${T}? e.g. def x[T: Tag, F[_]: TagK] = ..."

  final def tagFormatMap: Map[Kind, String] = {
    Map(
      Kind(Nil) -> "Tag",
      Kind(Kind(Nil) :: Nil) -> "TagK",
      Kind(Kind(Nil) :: Kind(Nil) :: Nil) -> "TagKK",
      Kind(Kind(Nil) :: Kind(Nil) :: Kind(Nil) :: Nil) -> "TagK3",
      Kind(Kind(Kind(Nil) :: Nil) :: Nil) -> "TagT",
      Kind(Kind(Kind(Nil) :: Nil) :: Kind(Nil) :: Nil) -> "TagTK",
      Kind(Kind(Kind(Nil) :: Nil) :: Kind(Nil) :: Kind(Nil) :: Nil) -> "TagTKK",
      Kind(Kind(Kind(Nil) :: Nil) :: Kind(Nil) :: Kind(Nil) :: Kind(Nil) :: Nil) -> "TagTK3"
    )
  }

  final def tagFormat(tpe: Universe#Type): String = {
    val kind = kindOf(tpe)
    tagFormatMap.get(kind) match {
      case Some(t) => s"$t[$tpe]"
      case _ => s"HKTag for $tpe of kind $kind"
    }
  }
}

@nowarn("msg=deprecated")
class TagLambdaMacro(override val c: whitebox.Context) extends TagMacro(c) {

  import c.universe._
  import c.universe.internal.decorators._

  def lambdaImpl: c.Tree = {
    val pos = c.macroApplication.pos

    val targetTpe = c
      .enclosingUnit.body.collect {
        case AppliedTypeTree(t, arg :: _) if t.exists(_.pos == pos) =>
          c.typecheck(
            tree = arg,
            mode = c.TYPEmode,
            pt = c.universe.definitions.NothingTpe,
            silent = false,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = true
          ).tpe
      }.headOption match {
      case None =>
        c.abort(
          c.enclosingPosition,
          "Couldn't find an the type that `Tag.auto.T` macro was applied to, please make sure you use the correct syntax, as in `def tagk[F[_]: Tag.auto.T]: TagK[T] = implicitly[Tag.auto.T[F]]`"
        )
      case Some(t) =>
        t
    }

    val kind = kindOf(targetTpe)

    logger.log(s"Found position $pos, target type $targetTpe, target kind $kind")

    val ctorParam = mkTypeParameter(NoSymbol, kind)
    val ArgStruct = mkHKTagArgStruct(ctorParam.asType.toType, kind)

    val resultType = c
      .typecheck(
        tq"{ type T[${c.internal.typeDef(ctorParam)}] = _root_.izumi.reflect.HKTag[$ArgStruct] }",
        c.TYPEmode,
        c.universe.definitions.NothingTpe,
        silent = false,
        withImplicitViewsDisabled = true,
        withMacrosDisabled = true
      ).tpe

    val res = Literal(Constant(())).setType(resultType)

    logger.log(s"final result: $resultType")

    res
  }
}
