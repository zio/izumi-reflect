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
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymLiteral
import izumi.reflect.macrortti.LightTypeTagRef._

trait LTTRenderables extends Serializable with WithRenderableSyntax {

  def r_SymName(sym: SymName, hasPrefix: Boolean): String
  def prefixSplitter: String = "::"

  implicit lazy val r_LightTypeTag: Renderable[LightTypeTagRef] = new Renderable[LightTypeTagRef] {
    override def render(value: LightTypeTagRef): String = value match {
      case a: AbstractReference =>
        a.render()
    }
  }

  implicit lazy val r_AbstractReference: Renderable[AbstractReference] = new Renderable[AbstractReference] {
    override def render(value: AbstractReference): String = value match {
      case a: AppliedReference =>
        a.render()
      case l: Lambda =>
        l.render()
    }
  }

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

  implicit lazy val r_Refinement: Renderable[Refinement] = new Renderable[Refinement] {
    override def render(value: Refinement): String = {
      s"(${value.reference.render()} ${value.decls.toSeq.sorted(OrderingRefinementDeclInstance).map(_.render()).mkString("{", ", ", "}")})"
    }
  }

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

  implicit lazy val r_RefinementDecl: Renderable[RefinementDecl] = new Renderable[RefinementDecl] {
    override def render(value: RefinementDecl): String = value match {
      case RefinementDecl.Signature(name, input, output) =>
        s"def $name${input.map(_.render()).mkString("(", ", ", ")")}: ${output.render()}"
      case RefinementDecl.TypeMember(name, tpe) =>
        s"type $name = $tpe"
    }
  }

  implicit lazy val r_AppliedNamedReference: Renderable[AppliedNamedReference] = new Renderable[AppliedNamedReference] {
    override def render(value: AppliedNamedReference): String = value match {
      case n: NameReference =>
        n.render()
      case f: FullReference =>
        f.render()
    }
  }

  implicit lazy val r_Lambda: Renderable[Lambda] = new Renderable[Lambda] {
    override def render(value: Lambda): String = {
      s"λ ${value.input.map(_.render()).map(p => s"%$p").mkString(",")} → ${value.output.render()}"
    }
  }

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

  implicit lazy val r_FullReference: Renderable[FullReference] = new Renderable[FullReference] {
    override def render(value: FullReference): String = {
      s"${value.asName.render()}${value.parameters.map(_.render()).mkString("[", ",", "]")}"
    }
  }

  implicit lazy val r_IntersectionReference: Renderable[IntersectionReference] = new Renderable[IntersectionReference] {
    override def render(value: IntersectionReference): String = {
      value.refs.toSeq.sorted(OrderingAbstractReferenceInstance).map(_.render()).mkString("{", " & ", "}")
    }
  }

  implicit lazy val r_UnionReference: Renderable[UnionReference] = new Renderable[UnionReference] {
    override def render(value: UnionReference): String = {
      value.refs.toSeq.sorted(OrderingAbstractReferenceInstance).map(_.render()).mkString("{", " | ", "}")
    }
  }

  implicit lazy val r_TypeParam: Renderable[TypeParam] = new Renderable[TypeParam] {
    override def render(value: TypeParam): String = {
      s"${value.variance.render()}${value.ref.render()}"
    }
  }

  implicit lazy val r_Variance: Renderable[Variance] = new Renderable[Variance] {
    override def render(value: Variance): String = value match {
      case Variance.Invariant => "="
      case Variance.Contravariant => "-"
      case Variance.Covariant => "+"
    }
  }

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

object LTTRenderables {

  // omit package names
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

  // print package names
  object Long extends LTTRenderables {
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
  object LongPrefixDot extends LTTRenderables {
    override def r_SymName(sym: SymName, hasPrefix: Boolean): String = {
      Long.r_SymName(sym, hasPrefix)
    }
    override def prefixSplitter: String = "."
  }

}
