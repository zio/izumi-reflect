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

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import izumi.reflect.DebugProperties
import izumi.reflect.internal.OrderingCompat.setToSortedSet
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{SymLiteral, SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.UnpickleState

import scala.annotation.nowarn
import scala.collection.immutable.SortedSet

/**
  * Extracts internal databases from [[LightTypeTag]].
  * Should not be used under normal circumstances.
  *
  * Internal API: binary compatibility not guaranteed.
  */
case class LightTypeTagUnpacker(tag: LightTypeTag) {
  def bases: Map[AbstractReference, Set[AbstractReference]] = tag.basesdb

  def inheritance: Map[NameReference, Set[NameReference]] = tag.idb
}

abstract class LightTypeTag(
  bases: () => Map[AbstractReference, Set[AbstractReference]],
  inheritanceDb: () => Map[NameReference, Set[NameReference]]
) extends Serializable {

  def ref: LightTypeTagRef

  private[macrortti] lazy val basesdb: Map[AbstractReference, Set[AbstractReference]] = bases()
  private[macrortti] lazy val idb: Map[NameReference, Set[NameReference]] = inheritanceDb()

  @inline final def <:<(maybeParent: LightTypeTag): Boolean = {
    new LightTypeTagInheritance(this, maybeParent).isChild()
  }

  @inline final def =:=(other: LightTypeTag): Boolean = {
    this == other
  }

  final def decompose: Set[LightTypeTag] = {
    ref match {
      case LightTypeTagRef.IntersectionReference(refs) =>
        refs.map(r => LightTypeTag(r, basesdb, idb))
      case _ =>
        Set(this)
    }
  }

  /**
    * Parameterize this type tag with `args` if it describes an unapplied type lambda
    *
    * If there are less `args` given than this type takes parameters, it will remain a type
    * lambda taking remaining arguments:
    *
    * {{{
    *   F[?, ?, ?].combine(A, B) = F[A, B, ?]
    * }}}
    */
  def combine(args: LightTypeTag*): LightTypeTag = {
    val argRefs = args.map(_.ref)
    val appliedBases = basesdb.map {
      case (self: LightTypeTagRef.Lambda, parents) =>
        self.combine(argRefs) -> parents.map {
          case l: LightTypeTagRef.Lambda =>
            l.combine(argRefs)
          case o =>
            val context = self.input.map(_.name).zip(argRefs.collect { case a: AbstractReference => a }).toMap
            val out = new RuntimeAPI.Rewriter(context).replaceRefs(o)
            out
        }
      case o => o
    }

    def mergedBasesDB = LightTypeTag.mergeIDBs(appliedBases, args.iterator.map(_.basesdb))

    def mergedInheritanceDb = LightTypeTag.mergeIDBs(idb, args.iterator.map(_.idb))

    LightTypeTag(ref.combine(argRefs), mergedBasesDB, mergedInheritanceDb)
  }

  /**
    * Parameterize this type tag with `args` if it describes an unapplied type lambda
    *
    * The resulting type lambda will take parameters in places where `args` was None:
    *
    * {{{
    *   F[?, ?, ?].combine(Some(A), None, Some(C)) = F[A, ?, C]
    * }}}
    */
  def combineNonPos(args: Option[LightTypeTag]*): LightTypeTag = {
    val argRefs = args.map(_.map(_.ref))
    val appliedBases = basesdb ++ basesdb.map {
      case (self: LightTypeTagRef.Lambda, parents) =>
        self.combineNonPos(argRefs) -> parents.map {
          case l: LightTypeTagRef.Lambda =>
            l.combineNonPos(argRefs)
          case o => o
        }
      case o => o
    }

    def mergedBasesDB = LightTypeTag.mergeIDBs(appliedBases, args.iterator.map(_.map(_.basesdb).getOrElse(Map.empty)))

    def mergedInheritanceDb = LightTypeTag.mergeIDBs(idb, args.iterator.map(_.map(_.idb).getOrElse(Map.empty)))

    LightTypeTag(ref.combineNonPos(argRefs), mergedBasesDB, mergedInheritanceDb)
  }

  /**
    * Strip all args from type tag of parameterized type and its supertypes
    * Useful for very rough type-constructor / class-only comparisons.
    *
    * NOTE: This DOES NOT RESTORE TYPE CONSTRUCTOR/LAMBDA and is
    * NOT equivalent to .typeConstructor call in scala-reflect
    *       - You won't be able to call [[combine]] on result type
    * and partially applied types will not work correctly
    */
  @nowarn("msg=view.mapValues")
  def withoutArgs: LightTypeTag = {
    LightTypeTag(ref.withoutArgs, basesdb.mapValues(_.map(_.withoutArgs)).toMap, idb)
  }

  /**
    * Extract arguments applied to this type constructor
    */
  def typeArgs: List[LightTypeTag] = {
    ref.typeArgs.map(LightTypeTag(_, basesdb, idb))
  }

  /** Render to string, omitting package names */
  override def toString: String = {
    ref.toString
  }

  /** Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  def repr: String = {
    import izumi.reflect.macrortti.LTTRenderables.Long._
    ref.render()
  }

  /** Short class or type-constructor name of this type, without package or prefix names */
  def shortName: String = {
    ref.shortName
  }

  /** Class or type-constructor name of this type, WITH package name, but without prefix names */
  def longName: String = {
    ref.longName
  }

  /** Print internal structures state */
  @nowarn("msg=view.mapValues")
  def debug(name: String = ""): String = {
    import izumi.reflect.internal.fundamentals.platform.strings.IzString._
    s"""⚙️ $name: ${this.toString}
      |⚡️bases: ${basesdb.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}
      |⚡️inheritance: ${idb.mapValues(_.niceList(prefix = "* ").shift(2)).niceList()}
      |⚙️ end $name""".stripMargin
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: LightTypeTag =>
        ref == that.ref
      case _ => false
    }
  }

  override def hashCode(): Int = hashcode

  private[this] lazy val hashcode: Int = {
    ref.hashCode() * 31
  }
}

object LightTypeTag {
  @inline def apply(ref0: LightTypeTagRef, bases: => Map[AbstractReference, Set[AbstractReference]], db: => Map[NameReference, Set[NameReference]]): LightTypeTag = {
    new LightTypeTag(() => bases, () => db) {
      override final val ref: LightTypeTagRef = ref0
    }
  }

  /** Create a [[LightTypeTag]] formed of `intersection` with the structural refinement taken from `structure`
    *
    * @param structure the non-structural part of structure is ignored, except SubtypeDBs
    * @param additionalTypeMembers additional type members
    */
  def refinedType(intersection: List[LightTypeTag], structure: LightTypeTag, additionalTypeMembers: Map[String, LightTypeTag]): LightTypeTag = {
    val parts = intersection.iterator.flatMap(_.ref.decompose).toSet
    val intersectionRef = LightTypeTagRef.maybeIntersection(parts)
    val ref = {
      val decls = structure.ref match {
        case LightTypeTagRef.Refinement(_, decls) if decls.nonEmpty => decls
        case _ => Set.empty
      }
      if (decls.nonEmpty || additionalTypeMembers.nonEmpty) {
        val newDecls = decls.filterNot(additionalTypeMembers contains _.name) ++ additionalTypeMembers.iterator.map {
          case (k, v) =>
            RefinementDecl.TypeMember(k, v.ref match { case r: AbstractReference => r })
        }
        LightTypeTagRef.Refinement(intersectionRef, newDecls)
      } else {
        intersectionRef
      }
    }

    def mergedBasesDB: Map[AbstractReference, Set[AbstractReference]] =
      LightTypeTag.mergeIDBs(structure.basesdb, intersection.iterator.map(_.basesdb) ++ additionalTypeMembers.iterator.map(_._2.basesdb))

    def mergedInheritanceDb: Map[NameReference, Set[NameReference]] =
      LightTypeTag.mergeIDBs(structure.idb, intersection.iterator.map(_.idb) ++ additionalTypeMembers.iterator.map(_._2.idb))

    LightTypeTag(ref, mergedBasesDB, mergedInheritanceDb)
  }

  @deprecated("Binary compatibility for 1.0.0-M6+", "1.0.0-M6")
  private[reflect] def refinedType(intersection: List[LightTypeTag], structure: LightTypeTag): LightTypeTag = {
    refinedType(intersection, structure, Map.empty)
  }

  def parse[T](hashCode: Int, refString: String, basesString: String, version: Int): LightTypeTag = {
    lazy val shared = {
      subtypeDBsSerializer.unpickle(UnpickleState(ByteBuffer.wrap(basesString.getBytes(StandardCharsets.ISO_8859_1))))
    }

    if (version == 0) {
      new ParsedLightTypeTag(hashCode, refString, () => shared.bases, () => shared.idb)
    } else if (version >= 1 && version <= 10) {
      new ParsedLightTypeTagM8(hashCode, refString, () => shared.bases, () => shared.idb)
    } else {
      throw new LinkageError(s"""Couldn't parse a `LightTypeTag` version=$version generated by a newer version of `izumi-reflect`,
        |please include a newer version of the library jar `dev.zio:izumi-reflect`!
        |
        |Supported `version` range in this version: 0..10""".stripMargin)
    }
  }

  /** Old `ParsedLightTypeTag` generated before `1.0.0-M8`, must be kept for bincompat */
  final class ParsedLightTypeTag(
    override val hashCode: Int,
    private val refString: String,
    bases: () => Map[AbstractReference, Set[AbstractReference]],
    db: () => Map[NameReference, Set[NameReference]]
  ) extends LightTypeTag(bases, db) {
    override lazy val ref: LightTypeTagRef = {
      lttRefSerializer.unpickle(UnpickleState(ByteBuffer.wrap(refString.getBytes(StandardCharsets.ISO_8859_1))))
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: ParsedLightTypeTag if refString == that.refString =>
          true
        case _ =>
          super.equals(other)
      }
    }
  }
  object ParsedLightTypeTag {
    final case class SubtypeDBs(bases: Map[AbstractReference, Set[AbstractReference]], idb: Map[NameReference, Set[NameReference]])
  }

  /** `ParsedLightTypeTag` since 1.0.0-M8 */
  final class ParsedLightTypeTagM8(
    override val hashCode: Int,
     val refString: String,
    bases: () => Map[AbstractReference, Set[AbstractReference]],
    db: () => Map[NameReference, Set[NameReference]]
  ) extends LightTypeTag(bases, db) {
    override lazy val ref: LightTypeTagRef = {
      lttRefSerializer.unpickle(UnpickleState(ByteBuffer.wrap(refString.getBytes(StandardCharsets.ISO_8859_1))))
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: ParsedLightTypeTagM8 =>
          if (refString == that.refString) true
          else super.equals(other)
        case _ =>
          super.equals(other)
      }
    }
  }

  private[this] final val optimisticEqualsEnabled = {
    System
      .getProperty(DebugProperties.`izumi.reflect.rtti.optimized.equals`)
      .asBoolean()
      .getOrElse(true)
  }

  private[reflect] val (lttRefSerializer: Pickler[LightTypeTagRef], subtypeDBsSerializer: Pickler[SubtypeDBs]) = {
    import izumi.reflect.thirdparty.internal.boopickle
    import izumi.reflect.thirdparty.internal.boopickle.BasicPicklers.IntPickler
    import izumi.reflect.thirdparty.internal.boopickle.NoMacro.{Pickler => _, _}

    implicit lazy val variance: Pickler[Variance] = IntPickler.xmap({
      case 0 => Variance.Invariant: Variance
      case 1 => Variance.Contravariant: Variance
      case 2 => Variance.Covariant: Variance
    })({
      case Variance.Invariant => 0
      case Variance.Contravariant => 1
      case Variance.Covariant => 2
    })
    implicit lazy val symName: Pickler[SymName] = new Pickler[SymName] {
      override def pickle(obj: SymName)(implicit state: PickleState): Unit = {
        obj match {
          case SymTermName(name) =>
            Tuple2Pickler[Int, String].pickle((0, name))
          case SymTypeName(name) =>
            Tuple2Pickler[Int, String].pickle((1, name))
          case SymLiteral(name) =>
            Tuple2Pickler[Int, String].pickle((2, name))
        }
      }

      override def unpickle(implicit state: UnpickleState): SymName = Tuple2Pickler[Int, String].unpickle match {
        case (0, name) =>
          SymTermName(name)
        case (1, name) =>
          SymTypeName(name)
        case (2, name) =>
          SymLiteral(name)
        case o =>
          throw new IllegalArgumentException(s"Unexpected data: $o")
      }
    }

    implicit lazy val boundariesDefined: Pickler[Boundaries.Defined] = new Pickler[Boundaries.Defined] {
      override def pickle(obj: Boundaries.Defined)(implicit state: PickleState): Unit = {
        Tuple2Pickler[AbstractReference, AbstractReference].pickle((obj.bottom, obj.top))
      }

      override def unpickle(implicit state: UnpickleState): Boundaries.Defined = {
        val u = Tuple2Pickler[AbstractReference, AbstractReference].unpickle
        Boundaries.Defined(u._1, u._2)
      }
    }
    implicit lazy val boundaries: Pickler[Boundaries] = new Pickler[Boundaries] {
      override def pickle(obj: Boundaries)(implicit state: PickleState): Unit = obj match {
        case d: Boundaries.Defined =>
          optionPickler[Boundaries.Defined].pickle(Some(d))
        case Boundaries.Empty =>
          optionPickler[Boundaries.Defined].pickle(None)
      }

      override def unpickle(implicit state: UnpickleState): Boundaries = {
        optionPickler[Boundaries.Defined].unpickle match {
          case Some(value) =>
            value
          case None =>
            Boundaries.Empty
        }
      }
    }

    implicit def nameRefSerializer: Pickler[NameReference] = new boopickle.Pickler[LightTypeTagRef.NameReference] {
      override def pickle(value: LightTypeTagRef.NameReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[LightTypeTagRef.SymName](value.ref)
            state.pickle[LightTypeTagRef.Boundaries](value.boundaries)
            state.pickle[Option[LightTypeTagRef.AppliedReference]](value.prefix)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.NameReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = new LightTypeTagRef.NameReference(
            state.unpickle[LightTypeTagRef.SymName],
            state.unpickle[LightTypeTagRef.Boundaries],
            state.unpickle[Option[LightTypeTagRef.AppliedReference]]
          )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.NameReference](-ic)
        else
          state.codingError(ic)
      }
    }

    implicit lazy val appliedref: Pickler[AppliedReference] = new boopickle.CompositePickler[LightTypeTagRef.AppliedReference] {
      addConcreteType[FullReference]
      addConcreteType[IntersectionReference]
      addConcreteType[NameReference]
      addConcreteType[Refinement]
      addConcreteType[UnionReference]
    }
    implicit lazy val aref: Pickler[AbstractReference] = new boopickle.CompositePickler[LightTypeTagRef.AbstractReference] {
      addConcreteType[FullReference]
      addConcreteType[IntersectionReference]
      addConcreteType[LightTypeTagRef.Lambda]
      addConcreteType[NameReference]
      addConcreteType[Refinement]
      addConcreteType[UnionReference]
    }
    implicit lazy val tagref: Pickler[LightTypeTagRef] = new boopickle.CompositePickler[LightTypeTagRef] {
      addConcreteType[FullReference]
      addConcreteType[IntersectionReference]
      addConcreteType[LightTypeTagRef.Lambda]
      addConcreteType[NameReference]
      addConcreteType[Refinement]
      addConcreteType[UnionReference]
    }
    implicit lazy val fullRef: Pickler[FullReference] = new boopickle.Pickler[LightTypeTagRef.FullReference] {
      override def pickle(value: LightTypeTagRef.FullReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[String](value.ref)
            state.pickle[List[LightTypeTagRef.TypeParam]](value.parameters)
            state.pickle[Option[LightTypeTagRef.AppliedReference]](value.prefix)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.FullReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.FullReference(
            state.unpickle[String],
            state.unpickle[List[LightTypeTagRef.TypeParam]],
            state.unpickle[Option[LightTypeTagRef.AppliedReference]]
          )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.FullReference](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val typeParam: Pickler[LightTypeTagRef.TypeParam] = new boopickle.Pickler[LightTypeTagRef.TypeParam] {
      override def pickle(value: LightTypeTagRef.TypeParam)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[LightTypeTagRef.AbstractReference](value.ref)
            state.pickle[LightTypeTagRef.Variance](value.variance)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.TypeParam = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.TypeParam(state.unpickle[LightTypeTagRef.AbstractReference], state.unpickle[LightTypeTagRef.Variance])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.TypeParam](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val union: Pickler[UnionReference] = new boopickle.Pickler[LightTypeTagRef.UnionReference] {
      override def pickle(value: LightTypeTagRef.UnionReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[SortedSet[LightTypeTagRef.AppliedReference]](setToSortedSet[LightTypeTagRef.AppliedReference](OrderingAbstractReferenceInstance)(value.refs))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.UnionReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.UnionReference(state.unpickle[SortedSet[LightTypeTagRef.AppliedReference]])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.UnionReference](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val intersection: Pickler[IntersectionReference] = new boopickle.Pickler[LightTypeTagRef.IntersectionReference] {
      override def pickle(value: LightTypeTagRef.IntersectionReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[SortedSet[LightTypeTagRef.AppliedReference]](setToSortedSet(OrderingAbstractReferenceInstance[LightTypeTagRef.AppliedReference])(value.refs))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.IntersectionReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.IntersectionReference(state.unpickle[SortedSet[LightTypeTagRef.AppliedReference]])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.IntersectionReference](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val lambda: Pickler[LightTypeTagRef.Lambda] = new boopickle.Pickler[LightTypeTagRef.Lambda] {
      override def pickle(value: LightTypeTagRef.Lambda)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[List[LightTypeTagRef.LambdaParameter]](value.input)
            state.pickle[LightTypeTagRef.AbstractReference](value.output)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.Lambda = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.Lambda(state.unpickle[List[LightTypeTagRef.LambdaParameter]], state.unpickle[LightTypeTagRef.AbstractReference])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.Lambda](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val lambdaParameter: Pickler[LightTypeTagRef.LambdaParameter] = new boopickle.Pickler[LightTypeTagRef.LambdaParameter] {
      override def pickle(value: LightTypeTagRef.LambdaParameter)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[String](value.name)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.LambdaParameter = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.LambdaParameter(state.unpickle[String])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.LambdaParameter](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val refinement: Pickler[Refinement] = new boopickle.Pickler[LightTypeTagRef.Refinement] {
      override def pickle(value: LightTypeTagRef.Refinement)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[LightTypeTagRef.AppliedReference](value.reference)
            state.pickle[SortedSet[LightTypeTagRef.RefinementDecl]](setToSortedSet(OrderingRefinementDecl)(value.decls))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.Refinement = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.Refinement(state.unpickle[LightTypeTagRef.AppliedReference], state.unpickle[SortedSet[LightTypeTagRef.RefinementDecl]])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.Refinement](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val refinementDecl: boopickle.CompositePickler[LightTypeTagRef.RefinementDecl] = new boopickle.CompositePickler[LightTypeTagRef.RefinementDecl] {
      addConcreteType[LightTypeTagRef.RefinementDecl.Signature]
      addConcreteType[LightTypeTagRef.RefinementDecl.TypeMember]
    }

    implicit lazy val typeMember: Pickler[RefinementDecl.TypeMember] = new boopickle.Pickler[LightTypeTagRef.RefinementDecl.TypeMember] {
      override def pickle(value: LightTypeTagRef.RefinementDecl.TypeMember)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[String](value.name)
            state.pickle[LightTypeTagRef.AbstractReference](value.ref)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.RefinementDecl.TypeMember = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.RefinementDecl.TypeMember(state.unpickle[String], state.unpickle[LightTypeTagRef.AbstractReference])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.RefinementDecl.TypeMember](-ic)
        else
          state.codingError(ic)
      }
    }
    implicit lazy val signature: Pickler[RefinementDecl.Signature] = new boopickle.Pickler[LightTypeTagRef.RefinementDecl.Signature] {
      override def pickle(value: LightTypeTagRef.RefinementDecl.Signature)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined)
            state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[String](value.name)
            state.pickle[List[LightTypeTagRef.AppliedReference]](value.input)
            state.pickle[LightTypeTagRef.AppliedReference](value.output)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.RefinementDecl.Signature = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef
            .RefinementDecl.Signature(
              state.unpickle[String],
              state.unpickle[List[LightTypeTagRef.AppliedReference]],
              state.unpickle[LightTypeTagRef.AppliedReference]
            )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.RefinementDecl.Signature](-ic)
        else
          state.codingError(ic)
      }
    }

    implicit lazy val dbsSerializer: Pickler[SubtypeDBs] = new boopickle.Pickler[LightTypeTag.ParsedLightTypeTag.SubtypeDBs] {
      override def pickle(value: LightTypeTag.ParsedLightTypeTag.SubtypeDBs)(implicit state: boopickle.PickleState): Unit = {

        val ref = state.identityRefFor(value)
        if (ref.isDefined)
          state.enc.writeInt(-ref.get)
        else {
          state.enc.writeInt(0)
          state.pickle[Map[LightTypeTagRef.AbstractReference, Set[LightTypeTagRef.AbstractReference]]](value.bases)
          state.pickle[Map[LightTypeTagRef.NameReference, Set[LightTypeTagRef.NameReference]]](value.idb)
          state.addIdentityRef(value)
        }

        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTag.ParsedLightTypeTag.SubtypeDBs = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTag
            .ParsedLightTypeTag.SubtypeDBs(
              state.unpickle[Map[LightTypeTagRef.AbstractReference, Set[LightTypeTagRef.AbstractReference]]],
              state.unpickle[Map[LightTypeTagRef.NameReference, Set[LightTypeTagRef.NameReference]]]
            )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTag.ParsedLightTypeTag.SubtypeDBs](-ic)
        else
          state.codingError(ic)
      }
    }

    // false positive unused warnings
    //lazy val _ = (symTypeName, symTermName, symName, appliedRefSerializer, nameRefSerializer, abstractRefSerializer)

    (tagref, dbsSerializer)
  }

  private[macrortti] def mergeIDBs[T](self: Map[T, Set[T]], other: Map[T, Set[T]]): Map[T, Set[T]] = {
    import izumi.reflect.internal.fundamentals.collections.IzCollections._

    val both = self.toSeq ++ other.toSeq
    both.toMultimap.map {
      case (k, v) =>
        (k, v.flatten.filterNot(_ == k))
    }
  }

  private[macrortti] def mergeIDBs[T](self: Map[T, Set[T]], others: Iterator[Map[T, Set[T]]]): Map[T, Set[T]] = {
    others.foldLeft(self)(mergeIDBs[T])
  }

}
