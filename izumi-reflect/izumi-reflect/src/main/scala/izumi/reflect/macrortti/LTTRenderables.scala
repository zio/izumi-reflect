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

import izumi.reflect.internal.fundamentals.functional.{Renderable, WithRenderableSyntax}
import izumi.reflect.internal.fundamentals.platform.language.unused
import izumi.reflect.macrortti.LightTypeTagInheritance.{tpeAny, tpeAnyRef, tpeNothing, tpeNull, tpeObject}
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymLiteral
import izumi.reflect.macrortti.LightTypeTagRef._

/** Rules that render a `LightTypeTagRef` as a string, one `Renderable` per kind of node. The instances in the companion object differ in
  * how they print names, prefixes and lambdas: `Short`, `Long`, `LongPrefixDot`, `ScalaStyledLambdas` and `ScalaStyledLambdasLong`.
  */
trait LTTRenderables extends Serializable with WithRenderableSyntax {

  /** Renders the symbol name `sym`; `hasPrefix` is `true` when the name is printed after a prefix. */
  def r_SymName(sym: SymName, hasPrefix: Boolean): String

  /** The text between a prefix and a name: `::` by default, and `.` in `LongPrefixDot` and the `ScalaStyledLambdas` renderers. */
  def prefixSplitter: String = "::"

  /** Renders any `LightTypeTagRef`. */
  implicit lazy val r_LightTypeTag: Renderable[LightTypeTagRef] = new Renderable[LightTypeTagRef] {
    override def render(value: LightTypeTagRef): String = value match {
      case a: AbstractReference =>
        a.render()
    }
  }

  /** Renders a lambda or an applied reference. */
  implicit lazy val r_AbstractReference: Renderable[AbstractReference] = new Renderable[AbstractReference] {
    override def render(value: AbstractReference): String = value match {
      case a: AppliedReference =>
        a.render()
      case l: Lambda =>
        l.render()
    }
  }

  /** Renders a name, an intersection, a union, a refinement or a wildcard. */
  implicit lazy val r_AppliedReference: Renderable[AppliedReference] = new Renderable[AppliedReference] {
    override def render(value: AppliedReference): String = value match {
      case a: AppliedNamedReference =>
        a.render()
      case i: IntersectionReference =>
        i.render()
      case u: UnionReference =>
        u.render()
      case r: Refinement =>
        r.render()
      case r: WildcardReference =>
        r.render()
    }
  }

  /** Renders a refinement as `(base {decl, decl})`, such as `(CharSequence {def foo(Int, String): Long})`. */
  implicit lazy val r_Refinement: Renderable[Refinement] = new Renderable[Refinement] {
    override def render(value: Refinement): String = {
      s"(${value.reference.render()} ${value.decls.toSeq.sorted(OrderingRefinementDeclInstance).map(_.render()).mkString("{", ", ", "}")})"
    }
  }

  /** Renders a wildcard as `?`, or as `?: <bottom..top>` when it has bounds. */
  implicit lazy val r_Wildcard: Renderable[WildcardReference] = new Renderable[WildcardReference] {
    override def render(value: WildcardReference): String = {
      value.boundaries match {
        case _: Boundaries.Defined =>
          s"?: ${value.boundaries.render()}"

        case Boundaries.Empty =>
          "?"
      }
    }
  }

  /** Renders a refinement member as `def name(inputs): output` or as `type name = ref`. */
  implicit lazy val r_RefinementDecl: Renderable[RefinementDecl] = new Renderable[RefinementDecl] {
    override def render(value: RefinementDecl): String = value match {
      case RefinementDecl.Signature(name, input, output) =>
        s"def $name${input.map(_.render()).mkString("(", ", ", ")")}: ${output.render()}"
      case RefinementDecl.TypeMember(name, tpe) =>
        s"type $name = $tpe"
    }
  }

  /** Renders a [[LightTypeTagRef.NameReference]] or a [[LightTypeTagRef.FullReference]]. */
  implicit lazy val r_AppliedNamedReference: Renderable[AppliedNamedReference] = new Renderable[AppliedNamedReference] {
    override def render(value: AppliedNamedReference): String = value match {
      case n: NameReference =>
        n.render()
      case f: FullReference =>
        f.render()
    }
  }

  /** Renders a lambda as `λ %0,%1 → body`. */
  implicit lazy val r_Lambda: Renderable[Lambda] = new Renderable[Lambda] {
    override def render(value: Lambda): String = {
      s"λ ${value.input.map(_.render()).map(p => s"%$p").mkString(",")} → ${value.output.render()}"
    }
  }

  /** Renders a lambda parameter as its index, or as `depth:index` when its depth is positive. */
  implicit lazy val r_LambdaParameterName: Renderable[SymName.LambdaParamName] = new Renderable[SymName.LambdaParamName] {
    override def render(value: SymName.LambdaParamName): String = {
      value.depth match {
        case t if t <= 0 =>
          s"${value.index}"
        case t if t > 0 =>
          s"${value.depth}:${value.index}"
        // FIXME so-called "debug" view doesn't display all the data here which could lead to confusion when "debugging"
        //          s"[${value.arity}]${value.depth}:${value.index}"
      }
    }
  }

  /** Renders a name with its prefix and the splitter in front (`Obj::C`), and with `|<bottom..top>` after the name when the bounds are
    * defined (`HasT::T|<Int..AnyVal>`).
    */
  implicit lazy val r_NameRefRenderer: Renderable[NameReference] = new Renderable[NameReference] {
    override def render(value: NameReference): String = {
      val r = r_SymName(value.ref, value.prefix.isDefined)

      val rr = value.boundaries match {
        case _: Boundaries.Defined =>
          s"$r|${value.boundaries.render()}"
        case Boundaries.Empty =>
          r
      }

      value.prefix match {
        case Some(p) =>
          s"${p.render()}$prefixSplitter$rr"
        case None =>
          rr
      }
    }
  }

  /** Renders a name followed by its arguments in brackets, such as `List[+Int]`. */
  implicit lazy val r_FullReference: Renderable[FullReference] = new Renderable[FullReference] {
    override def render(value: FullReference): String = {
      s"${value.asName.render()}${value.parameters.map(_.render()).mkString("[", ",", "]")}"
    }
  }

  /** Renders an intersection as `{A & B}`, with the members in a fixed order. */
  implicit lazy val r_IntersectionReference: Renderable[IntersectionReference] = new Renderable[IntersectionReference] {
    override def render(value: IntersectionReference): String = {
      value.refs.toSeq.sorted(OrderingAbstractReferenceInstance).map(r => (r: AppliedReference).render()).mkString("{", " & ", "}")
    }
  }

  /** Renders a union as `{A | B}`, with the members in a fixed order. */
  implicit lazy val r_UnionReference: Renderable[UnionReference] = new Renderable[UnionReference] {
    override def render(value: UnionReference): String = {
      value.refs.toSeq.sorted(OrderingAbstractReferenceInstance).map(r => (r: AppliedReference).render()).mkString("{", " | ", "}")
    }
  }

  /** Renders a type argument as its variance followed by the type, such as `+Int`. */
  implicit lazy val r_TypeParam: Renderable[TypeParam] = new Renderable[TypeParam] {
    override def render(value: TypeParam): String = {
      s"${value.variance.render()}${value.ref.render()}"
    }
  }

  /** Renders a variance as `=` (invariant), `-` (contravariant) or `+` (covariant). */
  implicit lazy val r_Variance: Renderable[Variance] = new Renderable[Variance] {
    override def render(value: Variance): String = value match {
      case Variance.Invariant => "="
      case Variance.Contravariant => "-"
      case Variance.Covariant => "+"
    }
  }

  /** Renders defined bounds as `<bottom..top>` and empty bounds as an empty string. */
  implicit lazy val r_Boundaries: Renderable[Boundaries] = new Renderable[Boundaries] {
    override def render(value: Boundaries): String = value match {
      case Boundaries.Defined(bottom, top) =>
        s"<${bottom.render()}..${top.render()}>"

      case Boundaries.Empty =>
        ""
    }
  }

  @deprecated("bincompat only", "20.03.2023")
  private[macrortti] implicit lazy val r_LambdaParameter: Renderable[LambdaParameter] = new Renderable[LambdaParameter] {
    override def render(value: LambdaParameter): String = value match {
      case l: SymName.LambdaParamName => r_SymName(l, hasPrefix = false)
    }
  }

}

/** The renderers of `LightTypeTagRef`; see `LTTRenderables`. */
object LTTRenderables {

  // omit package names
  /** Omits package names. Prefixes stay, split by `::` (`Obj::C[=Int]`). `LightTypeTagRef#toString` uses it. */
  object Short extends LTTRenderables {
    override def r_SymName(sym: SymName, @unused hasPrefix: Boolean): String = {
      sym match {
        case SymLiteral(c) =>
          c
        case t: SymName.LambdaParamName =>
          t.render()
        case s: SymName.NamedSymbol =>
          s.name.split('.').last
      }
    }
  }

  /** Prints fully qualified names, with prefixes split by `::` (`Obj::C[=scala.Int]`). `LightTypeTagRef#repr` uses it. */
  object Long extends Long

  // print package names
  private[LTTRenderables] trait Long extends LTTRenderables {
    override def r_SymName(sym: SymName, hasPrefix: Boolean): String = {
      if (hasPrefix) {
        Short.r_SymName(sym, hasPrefix)
      } else {
        sym match {
          case t: SymName.LambdaParamName => t.render()
          case o: SymName.NamedSymbol => o.name
        }
      }
    }

    private[macrortti] def renderDb(db: Map[_ <: AbstractReference, Set[_ <: AbstractReference]]): String = {
      import izumi.reflect.internal.fundamentals.platform.strings.IzString._
      db.toList.sortBy(_._1)(OrderingAbstractReferenceInstance).map {
          case (k, v) => s"${k.repr} -> ${v.toList.sorted(OrderingAbstractReferenceInstance).map(_.repr).niceList(prefix = "* ").shift(2)}"
        }.niceList()
    }
  }

  // Same as `Long`, but split prefixes with . instead of ::
  /** Same as `Long`, but splits prefixes with `.` (`Obj.C[=scala.Int]`). `LightTypeTagRef#longNameWithPrefix` uses it. */
  object LongPrefixDot extends LTTRenderables {
    override def r_SymName(sym: SymName, hasPrefix: Boolean): String = {
      Long.r_SymName(sym, hasPrefix)
    }

    override def prefixSplitter: String = "."
  }

  /** Like `Long`, in Scala notation: prefixes split by `.`, no sign for invariant parameters, bounds as `<: top`, `>: bottom` or
    * `>: bottom <: top`, and a lambda whose body is a named type taking the lambda's parameters in order, each once, as that named type
    * with `_` in their place (`scala.util.Either[+_,+_]`, `scala.util.Either[+java.lang.String,+_]`). Other lambdas are rendered like
    * `ScalaStyledLambdasLong`. `LightTypeTagRef#scalaStyledRepr` uses it.
    */
  object ScalaStyledLambdas extends ScalaStyledLambdasShared {
    override implicit lazy val r_LambdaParameterName: Renderable[SymName.LambdaParamName] = new Renderable[SymName.LambdaParamName] {
      override def render(value: SymName.LambdaParamName): String = "_"
    }

    override implicit lazy val r_Lambda: Renderable[Lambda] = new Renderable[Lambda] {
      override def render(value: Lambda): String = {
        val isSimpleShape = value match {
          case Lambda(input, FullReference(_, tparams, _)) =>
            val (unusedRemaining, suspicious) = tparams.foldLeft((input, List.empty[AbstractReference])) {
              // params must be applied in definition order
              case ((all @ unusedArg :: tl, suspicious), TypeParam(p: AppliedNamedReference, variance)) =>
                if (p.symName == unusedArg) {
                  p match {
                    case _: NameReference =>
                      (tl, suspicious)
                    case FullReference(symName, parameters, prefix) =>
                      (tl, parameters.map(_.ref) ++ suspicious)
                  }
                } else {
                  (all, p :: suspicious)
                }
              case ((all, suspicious), TypeParam(other, _)) => (all, other :: suspicious)
            }
            val innerUses = suspicious.iterator.flatMap(RuntimeAPI.unpack).map(_.symName).toSet
            unusedRemaining.isEmpty && input.toSet.intersect(innerUses).isEmpty
          case _ =>
            false
        }
        if (isSimpleShape) {
          s"${value.output.render()}"
        } else {
          ScalaStyledLambdasLong.r_Lambda.render(value)
        }
      }
    }
  }

  /** Like `ScalaStyledLambdas`, but writes each lambda as `[A,B] ➾ body`, naming the parameters with letters. */
  object ScalaStyledLambdasLong extends ScalaStyledLambdasShared {
    override implicit lazy val r_LambdaParameterName: Renderable[SymName.LambdaParamName] = new Renderable[SymName.LambdaParamName] {
      override def render(value: SymName.LambdaParamName): String = {
        val char = ('A' + (value.index % 25)).toChar.toString
        val numChars = 1 + value.index / 25
        val suffix = if (value.depth > 0) s"${value.depth}" else ""
        s"${char * numChars}$suffix"
      }
    }

    override implicit lazy val r_Lambda: Renderable[Lambda] = new Renderable[Lambda] {
      override def render(value: Lambda): String = {
        s"[${value.input.map(_.render()).mkString(",")}] ➾ ${value.output.render()}"
      }
    }
  }

  private[LTTRenderables] trait ScalaStyledLambdasShared extends Long {
    override def prefixSplitter: String = "."

    override implicit lazy val r_Variance: Renderable[Variance] = new Renderable[Variance] {
      override def render(value: Variance): String = value match {
        case Variance.Invariant => ""
        case Variance.Contravariant => "-"
        case Variance.Covariant => "+"
      }
    }

    override implicit lazy val r_NameRefRenderer: Renderable[NameReference] = new Renderable[NameReference] {
      override def render(value: NameReference): String = {
        val r = r_SymName(value.ref, value.prefix.isDefined)

        val rr = value.boundaries match {
          case _: Boundaries.Defined =>
            s"$r ${value.boundaries.render()}"
          case Boundaries.Empty =>
            r
        }

        value.prefix match {
          case Some(p) =>
            s"${p.render()}$prefixSplitter$rr"
          case None =>
            rr
        }
      }
    }

    override implicit lazy val r_Boundaries: Renderable[Boundaries] = new Renderable[Boundaries] {
      override def render(value: Boundaries): String = value match {
        case Boundaries.Defined(bottom, top) if bottom == tpeNothing || bottom == tpeNull => s"<: ${top.render()}"
        case Boundaries.Defined(bottom, top) if top == tpeAny || top == tpeAnyRef || top == tpeObject => s">: ${bottom.render()}"
        case Boundaries.Defined(bottom, top) => s">: ${bottom.render()} <: ${top.render()}"
        case Boundaries.Empty => ""
      }
    }
  }
}
