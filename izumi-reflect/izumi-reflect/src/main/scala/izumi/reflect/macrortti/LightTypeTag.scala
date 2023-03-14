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

import izumi.reflect.DebugProperties
import izumi.reflect.internal.OrderingCompat.ArraySeqLike
import izumi.reflect.internal.fundamentals.platform.strings.IzString.toRichString
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef.SymName.{LambdaParamName, SymLiteral, SymTermName, SymTypeName}
import izumi.reflect.macrortti.LightTypeTagRef._
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.UnpickleState

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.nowarn
import scala.collection.immutable.HashSet

/**
  * Extracts internal databases from [[LightTypeTag]].
  * Should not be used under normal circumstances.
  *
  * Internal API: binary compatibility not guaranteed.
  */
final case class LightTypeTagUnpacker(tag: LightTypeTag) {
  def bases: Map[AbstractReference, Set[AbstractReference]] = tag.basesdb
  def inheritance: Map[NameReference, Set[NameReference]] = tag.idb
}

abstract class LightTypeTag private[reflect] (
  bases: () => Map[AbstractReference, Set[AbstractReference]],
  inheritanceDb: () => Map[NameReference, Set[NameReference]]
) extends Serializable {

  def ref: LightTypeTagRef

  // full subtyping db with lambdas, parameters and variance, e.g. List[+A] <: SeqOps[A, List, List[A]], λ %0 → List[+%0] <: λ %0,%1,%2 → SeqOps[+%0, +%1, +%2]
  private[reflect] lazy val basesdb: Map[AbstractReference, Set[AbstractReference]] = bases()
  // class inheritance db without lambdas and without parameters, e.g. List <: SeqOps, Iterable
  private[reflect] lazy val idb: Map[NameReference, Set[NameReference]] = inheritanceDb()

  def binaryFormatVersion: Int

  @inline final def <:<(maybeParent: LightTypeTag): Boolean = {
    new LightTypeTagInheritance(this, maybeParent).isChild()
  }

  @inline final def =:=(other: LightTypeTag): Boolean = {
    this == other
  }

  final def decompose: Set[LightTypeTag] = {
    ref match {
      case LightTypeTagRef.IntersectionReference(refs) =>
        refs.map(LightTypeTag(_, basesdb, idb))
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
    val appliedBases = basesdb ++ basesdb.iterator.collect { // do not remove the unapplied base lambdas after combination (required for inferredLambdaParents in isChild)
      case (self: LightTypeTagRef.Lambda, parents) =>
        self.combine(argRefs) -> parents.map {
          case l: LightTypeTagRef.Lambda =>
            l.combine(argRefs)
          case nonLambdaParent =>
            val context = self.input.zip(argRefs.collect { case a: AbstractReference => a }).toMap
            new RuntimeAPI.Rewriter(context).replaceRefs(nonLambdaParent)
        }
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
    val appliedBases = basesdb ++ basesdb.iterator.collect { // do not remove the unapplied base lambdas after combination (required for inferredLambdaParents in isChild)
      case (self: LightTypeTagRef.Lambda, parents) =>
        self.combineNonPos(argRefs) -> parents.map {
          case l: LightTypeTagRef.Lambda =>
            l.combineNonPos(argRefs)
          case nonLambdaParent =>
            val context = self.input.zip(argRefs.flatten.collect { case a: AbstractReference => a }).toMap
            new RuntimeAPI.Rewriter(context).replaceRefs(nonLambdaParent)
        }
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
  @nowarn("msg=Unused import")
  def withoutArgs: LightTypeTag = {
    import scala.collection.compat._
    LightTypeTag(ref.withoutArgs, basesdb.view.mapValues(_.map(_.withoutArgs)).toMap, idb)
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

  /**
    * Fully-qualified rendering of a type, including packages and prefix types.
    * Use [[toString]] for a rendering that omits package names
    */
  def repr: String = {
    ref.repr
  }

  /** Short class or type-constructor name of this type, without package or prefix names */
  def shortName: String = {
    ref.shortName
  }

  /** Class or type-constructor name of this type, with package and prefix names */
  def longNameWithPrefix: String = {
    ref.longNameWithPrefix
  }

  /** Internal symbol name of type-constructor of this type, with package and containing definition names */
  def longNameInternalSymbol: String = {
    ref.longNameInternalSymbol
  }

  /**
    * Fully-qualified rendering of a type, including packages and prefix types.
    * Traditional Scala notation for lambdas, e.g. scala.util.Either[+scala.Int,+_]
    */
  def scalaStyledName: String = {
    ref.scalaStyledName
  }

  @deprecated(
    "Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior",
    "2.2.2"
  )
  /** @deprecated Produces Scala version dependent output, with incorrect prefixes for types with value prefixes. Use `longNameWithPrefix` instead, or `longNameInternalSymbol` for old behavior */
  def longName: String = {
    ref.longName
  }

  /** Print internal structures state */
  def debug(name: String = ""): String = {
    s"""⚙️ begin $name: ${this.repr}
       |⚡️bases:${LTTRenderables.Long.renderDb(basesdb)}
       |⚡️inheritance:${LTTRenderables.Long.renderDb(idb)}
       |⚙️ end $name""".stripMargin
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: LightTypeTag =>
        ref == that.ref
      case _ => false
    }
  }

  override def hashCode(): Int = {
    hashcode
  }

  private[this] lazy val hashcode: Int = {
    ref.hashCode() * 31
  }
}

object LightTypeTag {
  final val currentBinaryFormatVersion = 30

  @inline def apply(ref0: LightTypeTagRef, bases: => Map[AbstractReference, Set[AbstractReference]], db: => Map[NameReference, Set[NameReference]]): LightTypeTag = {
    new UnparsedLightTypeTag(ref0, () => bases, () => db)
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
        case LightTypeTagRef.Refinement(_, decls) if decls.nonEmpty =>
          decls
        case _ =>
          Set.empty[RefinementDecl]
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

  def unionType(union: List[LightTypeTag]): LightTypeTag = {
    val parts = union.iterator.flatMap(_.ref.decomposeUnion).toSet
    val ref = LightTypeTagRef.maybeUnion(parts)

    def mergedBasesDB: Map[AbstractReference, Set[AbstractReference]] =
      LightTypeTag.mergeIDBs(Map.empty[AbstractReference, Set[AbstractReference]], union.iterator.map(_.basesdb))

    def mergedInheritanceDb: Map[NameReference, Set[NameReference]] =
      LightTypeTag.mergeIDBs(Map.empty[NameReference, Set[NameReference]], union.iterator.map(_.idb))

    LightTypeTag(ref, mergedBasesDB, mergedInheritanceDb)
  }

  def parse[T](hashCode: Int, refString: String, basesString: String, version: Int): LightTypeTag = {
    lazy val shared = {
      subtypeDBsSerializer.unpickle(UnpickleState(ByteBuffer.wrap(basesString.getBytes(StandardCharsets.ISO_8859_1))))
    }

    if (version >= 0 && version < currentBinaryFormatVersion) {
      // because lambda parameter names in binary format are not normalized, the hashCode of the
      // binary string can/will violate equals/hashCode contract when comparing binary strings
      // between different binary version format or when using any hashCode from older binary
      // string. So we ignore all hashCodes and fast paths from old binary strings
      new ParsedLightTypeTagOldOrPre230(binaryFormatVersion = version, refString, () => shared.bases, () => shared.idb)
    } else if (version == currentBinaryFormatVersion) {
      new ParsedLightTypeTag230Plus(version, hashCode, refString, () => shared.bases, () => shared.idb)
    } else {
      throw new LinkageError(s"""Couldn't parse a `LightTypeTag` version=$version generated by a newer version of `izumi-reflect`,
                                |please include a newer version of the library jar `dev.zio:izumi-reflect`!
                                |
                                |Supported `version` range in this version: 0..$currentBinaryFormatVersion""".stripMargin)
    }
  }

  @deprecated("Binary compatibility for 1.0.0-M6+", "1.0.0-M6")
  private[reflect] def refinedType(intersection: List[LightTypeTag], structure: LightTypeTag): LightTypeTag = {
    refinedType(intersection, structure, Map.empty)
  }

  private[reflect] final class UnparsedLightTypeTag private[reflect] (
    override val ref: LightTypeTagRef,
    bases: () => Map[AbstractReference, Set[AbstractReference]],
    inheritanceDb: () => Map[NameReference, Set[NameReference]]
  ) extends LightTypeTag(bases, inheritanceDb) {
    @noinline override def binaryFormatVersion: Int = -1
  }

  private[reflect] object ParsedLightTypeTag {
    private[reflect] final case class SubtypeDBs private (
      bases: Map[AbstractReference, Set[AbstractReference]],
      idb: Map[NameReference, Set[NameReference]]
    )

    private[reflect] object SubtypeDBs {
      @nowarn("msg=Unused import")
      private[reflect] def make(bases: Map[AbstractReference, Set[AbstractReference]], idb: Map[NameReference, Set[NameReference]]): SubtypeDBs = {
        import scala.collection.compat._
        new SubtypeDBs(
          bases.view.mapValues(_.filterNot(v => LightTypeTagRef.isIgnored(v))).filterNot(_._2.isEmpty).toMap,
          idb.view.mapValues(_.filterNot(v => LightTypeTagRef.isIgnored(v))).filterNot(_._2.isEmpty).toMap
        )
      }
    }
  }

  /** `ParsedLightTypeTag` before 2.3.0 or before current version. It is forcefully deoptimized */
  private[reflect] final class ParsedLightTypeTagOldOrPre230 private[reflect] (
    // disable precomputed hashCode on older binary versions
    // (old serialized lambdas break equals-hashCode contract
    // when compared with new serialized lambdas)
//    override val hashCode: Int,
    override val binaryFormatVersion: Int,
    private[reflect] val refString: String,
    bases: () => Map[AbstractReference, Set[AbstractReference]],
    db: () => Map[NameReference, Set[NameReference]]
  ) extends LightTypeTag(bases, db) {
    override lazy val ref: LightTypeTagRef = {
      deserializeRefString(refString)
    }

    // disable optimizations for older binary versions
//    override def equals(other: Any): Boolean = {
//      other match {
//        case that: ParsedLightTypeTagPre230 if this.binaryFormatVersion == that.binaryFormatVersion =>
//          if (this.refString == that.refString) true
//          else if (optimisticEqualsEnabled) false
//          else super.equals(other)
//        case _ =>
//          super.equals(other)
//      }
//    }
  }

  /** `ParsedLightTypeTag` since 2.3.0 or current binary format version */
  private[reflect] final class ParsedLightTypeTag230Plus private[reflect] (
    override val binaryFormatVersion: Int,
    override val hashCode: Int,
    private[reflect] val refString: String,
    bases: () => Map[AbstractReference, Set[AbstractReference]],
    db: () => Map[NameReference, Set[NameReference]]
  ) extends LightTypeTag(bases, db) {
    override lazy val ref: LightTypeTagRef = {
      deserializeRefString(refString)
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: ParsedLightTypeTag230Plus if this.binaryFormatVersion == that.binaryFormatVersion =>
          if (this.refString == that.refString) true
          else if (optimisticEqualsEnabled) false
          else super.equals(other)
        case _ =>
          super.equals(other)
      }
    }
  }

  @noinline private[reflect] def deserializeRefString(refString: String): LightTypeTagRef = {
    lttRefSerializer.unpickle(UnpickleState(ByteBuffer.wrap(refString.getBytes(StandardCharsets.ISO_8859_1))))
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

    implicit lazy val variance: Pickler[Variance] = IntPickler.xmap {
      case 0 => Variance.Invariant: Variance
      case 1 => Variance.Contravariant: Variance
      case 2 => Variance.Covariant: Variance
    } {
      case Variance.Invariant => 0
      case Variance.Contravariant => 1
      case Variance.Covariant => 2
    }
    implicit lazy val symName: Pickler[SymName] = new Pickler[SymName] {
      override def pickle(obj: SymName)(implicit state: PickleState): Unit = {
        obj match {
          case SymTermName(name) =>
            state.enc.writeInt(0)
            state.pickle[String](name)
            ()

          case SymTypeName(name) =>
            state.enc.writeInt(1)
            state.pickle[String](name)
            ()

          case SymLiteral(name) =>
            state.enc.writeInt(2)
            state.pickle[String](name)
            ()

          case LambdaParamName(index, depth, arity) =>
            state.enc.writeInt(3)
            state.enc.writeInt(index)
            state.enc.writeInt(depth)
            state.enc.writeInt(arity)
            ()
        }
      }

      override def unpickle(implicit state: UnpickleState): SymName = state.dec.readInt match {
        case 0 =>
          SymTermName(state.unpickle[String])
        case 1 =>
          SymTypeName(state.unpickle[String])
        case 2 =>
          SymLiteral(state.unpickle[String])
        case 3 =>
          LambdaParamName(state.dec.readInt, state.dec.readInt, state.dec.readInt)
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
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
      addConcreteType[WildcardReference]
    }
    implicit lazy val aref: Pickler[AbstractReference] = new boopickle.CompositePickler[LightTypeTagRef.AbstractReference] {
      addConcreteType[FullReference]
      addConcreteType[IntersectionReference]
      addConcreteType[LightTypeTagRef.Lambda]
      addConcreteType[NameReference]
      addConcreteType[Refinement]
      addConcreteType[UnionReference]
      addConcreteType[WildcardReference]
    }
    implicit lazy val tagref: Pickler[LightTypeTagRef] = new boopickle.CompositePickler[LightTypeTagRef] {
      addConcreteType[FullReference]
      addConcreteType[IntersectionReference]
      addConcreteType[LightTypeTagRef.Lambda]
      addConcreteType[NameReference]
      addConcreteType[Refinement]
      addConcreteType[UnionReference]
      addConcreteType[WildcardReference]
    }

    implicit lazy val wildcardRefSerializer: Pickler[WildcardReference] = new boopickle.Pickler[LightTypeTagRef.WildcardReference] {
      override def pickle(value: LightTypeTagRef.WildcardReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[LightTypeTagRef.Boundaries](value.boundaries)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.WildcardReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = new WildcardReference(
            state.unpickle[Boundaries]
          )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.WildcardReference](-ic)
        else
          state.codingError(ic)
      }
    }

    // Deserializer for FullReference before version 2.3.0 (with String first parameter instead of SymName)
    implicit lazy val fullRef: Pickler[FullReference] = new boopickle.Pickler[LightTypeTagRef.FullReference] {
      override def pickle(value: LightTypeTagRef.FullReference)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            // After version 2.3.0 the FullReference format changed - we now use SymName not String for FullReference name
            // Thankfully, boopickle's identity serialization only uses number range Int.MinValue..0,
            // leaving us with all numbers above 0 to encode format variants.
            state.enc.writeInt(1) // We use 1 instead of normal value 0
            //
            state.pickle[SymName](value.symName)
            state.pickle[List[LightTypeTagRef.TypeParam]](value.parameters)
            state.pickle[Option[LightTypeTagRef.AppliedReference]](value.prefix)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.FullReference = {
        val ic = state.dec.readInt
        if (ic == 0) { // Pre 2.3.0 format
          val value = LightTypeTagRef.FullReference(
            SymName.SymTypeName(state.unpickle[String]),
            state.unpickle[List[LightTypeTagRef.TypeParam]],
            state.unpickle[Option[LightTypeTagRef.AppliedReference]]
          )
          state.addIdentityRef(value)
          value
        } else if (ic == 1) { // Post 2.3.0
          val value = LightTypeTagRef.FullReference(
            state.unpickle[SymName],
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[ArraySeqLike[LightTypeTagRef.AppliedReference]](LightTypeTagRef.refSetToSortedArray(value.refs))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.UnionReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.UnionReference(state.unpickle[HashSet[LightTypeTagRef.AppliedReference]])
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[ArraySeqLike[LightTypeTagRef.AppliedReference]](LightTypeTagRef.refSetToSortedArray(value.refs))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.IntersectionReference = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.IntersectionReference(state.unpickle[HashSet[LightTypeTagRef.AppliedReference]])
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            // After version 2.3.0 the Lambda format changed - we now use LambdaParamName not LambdaParameter for Lambda inputs
            // Thankfully, boopickle's identity serialization only uses number range Int.MinValue..0,
            // leaving us with all numbers above 0 to encode format variants.
            state.enc.writeInt(1) // We use 1 instead of normal value 0
            state.pickle[List[SymName.LambdaParamName]](value.input)
            state.pickle[LightTypeTagRef.AbstractReference](value.output)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.Lambda = {
        val ic = state.dec.readInt
        if (ic == 0) { // Pre 2.3.0 format
          object OldLambdaParameter {
            type OldLambdaParameter <: String

            implicit val oldLambdaParameterPickler: boopickle.Pickler[OldLambdaParameter] = new boopickle.Pickler[OldLambdaParameter] {
              override def pickle(obj: OldLambdaParameter.OldLambdaParameter)(implicit state: PickleState): Unit = {
                throw new RuntimeException("Impossible - old lambda parameter compat pickler should never be called to serialize - only to deserialize")
              }
              override def unpickle(implicit state: UnpickleState): OldLambdaParameter.OldLambdaParameter = {
                val ic = state.dec.readInt
                if (ic == 0) {
                  val value = state.unpickle[String]
                  state.addIdentityRef(value)
                  value.asInstanceOf[OldLambdaParameter]
                } else if (ic < 0)
                  state.identityFor[String](-ic).asInstanceOf[OldLambdaParameter]
                else
                  state.codingError(ic)
              }
            }
          }

          def convertPre230LambdaToNew(lambda: LightTypeTagRef.Lambda, paramMap: Map[String, SymName.LambdaParamName]): LightTypeTagRef.Lambda = {

            def goReplaceBoundaries(boundaries: Boundaries): Boundaries = boundaries match {
              case Boundaries.Defined(bottom, top) =>
                Boundaries.Defined(goReplace(bottom), goReplace(top))
              case Boundaries.Empty =>
                Boundaries.Empty
            }

            def goReplaceSymName(symName: SymName): SymName = symName match {
              case oldSymName @ SymTypeName(maybeLambdaParam) =>
                paramMap.getOrElse(maybeLambdaParam, oldSymName)
              case _ => symName
            }

            def goReplace[T <: AbstractReference](abstractReference: T): T = {
              (abstractReference match {
                case Lambda(input, output) =>
                  // a lambda read by another codec must have already
                  // been processed by conversion procedure, so it should
                  // have no clashes in SymTypeName with old parameters
                  // anymore and be safe to process
                  Lambda(input, goReplace(output))

                case IntersectionReference(refs) =>
                  IntersectionReference(refs.map(goReplace))

                case UnionReference(refs) =>
                  UnionReference(refs.map(goReplace))

                case WildcardReference(boundaries) =>
                  WildcardReference(goReplaceBoundaries(boundaries))

                case Refinement(reference, decls) =>
                  Refinement(
                    goReplace(reference),
                    decls.map {
                      case RefinementDecl.Signature(name, input, output) =>
                        RefinementDecl.Signature(name, input.map(goReplace), goReplace(output))
                      case RefinementDecl.TypeMember(name, ref) =>
                        RefinementDecl.TypeMember(name, goReplace(ref))
                    }
                  )

                case NameReference(ref, boundaries, prefix) =>
                  NameReference(goReplaceSymName(ref), goReplaceBoundaries(boundaries), prefix.map(goReplace))

                case FullReference(symName, parameters, prefix) =>
                  FullReference(
                    goReplaceSymName(symName),
                    parameters.map {
                      case TypeParam(ref, variance) => TypeParam(goReplace(ref), variance)
                    },
                    prefix.map(goReplace)
                  )
              }).asInstanceOf[T]
            }

            val LightTypeTagRef.Lambda(convertedParams, oldResult) = lambda
            LightTypeTagRef.Lambda(convertedParams, goReplace(oldResult))
          }

          import OldLambdaParameter.{OldLambdaParameter, oldLambdaParameterPickler}

          val oldParams = state.unpickle[List[OldLambdaParameter]]
          val lambdaResult = state.unpickle[LightTypeTagRef.AbstractReference]

          val convertedParams = oldParams.map(SymName.bincompatForceCreateLambdaParamNameFromString(_))
          val paramMap = oldParams.iterator.zip(convertedParams.iterator).toMap[String, SymName.LambdaParamName]

          val oldLambda = LightTypeTagRef.Lambda(convertedParams, lambdaResult)
          val value = convertPre230LambdaToNew(oldLambda, paramMap)
          state.addIdentityRef(value)
          value
        } else if (ic == 1) { // Post 2.3.0
          val value = LightTypeTagRef.Lambda(
            state.unpickle[List[SymName.LambdaParamName]],
            state.unpickle[LightTypeTagRef.AbstractReference]
          )
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[LightTypeTagRef.Lambda](-ic)
        else
          state.codingError(ic)
      }
    }

    implicit lazy val lambdaParameter: Pickler[SymName.LambdaParamName] = new boopickle.Pickler[SymName.LambdaParamName] {
      override def pickle(value: SymName.LambdaParamName)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[Int](value.index)
            state.pickle[Int](value.depth)
            state.pickle[Int](value.arity)
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): SymName.LambdaParamName = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = SymName.LambdaParamName(state.unpickle[Int], state.unpickle[Int], state.unpickle[Int])
          state.addIdentityRef(value)
          value
        } else if (ic < 0)
          state.identityFor[SymName.LambdaParamName](-ic)
        else
          state.codingError(ic)
      }
    }

    implicit lazy val refinement: Pickler[Refinement] = new boopickle.Pickler[LightTypeTagRef.Refinement] {
      override def pickle(value: LightTypeTagRef.Refinement)(implicit state: boopickle.PickleState): Unit = {
        {
          val ref = state.identityRefFor(value)
          if (ref.isDefined) state.enc.writeInt(-ref.get)
          else {
            state.enc.writeInt(0)
            state.pickle[LightTypeTagRef.AppliedReference](value.reference)
            state.pickle[ArraySeqLike[LightTypeTagRef.RefinementDecl]](LightTypeTagRef.refinementDeclSetToSortedArray(value.decls))
            state.addIdentityRef(value)
          }
        }
        ()
      }

      override def unpickle(implicit state: boopickle.UnpickleState): LightTypeTagRef.Refinement = {
        val ic = state.dec.readInt
        if (ic == 0) {
          val value = LightTypeTagRef.Refinement(state.unpickle[LightTypeTagRef.AppliedReference], state.unpickle[HashSet[LightTypeTagRef.RefinementDecl]])
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
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
          if (ref.isDefined) state.enc.writeInt(-ref.get)
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
        if (ref.isDefined) state.enc.writeInt(-ref.get)
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
            .ParsedLightTypeTag.SubtypeDBs.make(
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
    // lazy val _ = (symTypeName, symTermName, symName, appliedRefSerializer, nameRefSerializer, abstractRefSerializer)

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
