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

package izumi.reflect.test

import izumi.reflect.macrortti.{LTT, LTag, LightTypeTag}
import org.scalatest.wordspec.AnyWordSpec

trait TagAssertions extends AnyWordSpec with TagLogging {

  def assertRepr(t: LightTypeTag, expected: String): Unit = {
    assert(t.toString == expected); ()
  }

  def assertDebugSame(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val tDebug = t.debug("assert")
    val expectedDebug = expected.debug("assert")
    val clue = s"${t.repr} debug== ${expected.repr}"
    def failClue = s"1: $tDebug\n2: $expectedDebug"
    info(clue)
    assert(tDebug == expectedDebug, s"$clue\n$failClue"); ()
  }

  def assertSame(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val clue = s"${t.repr} =?= ${expected.repr}"
    info(clue)
    assert(t =:= expected, clue)
    assert(t.ref == expected.ref, s"ref: $clue"); ()
  }

  def assertSameRef(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val clue = s"${t.repr} =?= ${expected.repr}"
    info(clue)
    assert(t.ref == expected.ref, s"ref: $clue"); ()
  }

  def assertDifferent(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val clue = s"${t.repr} =!= ${expected.repr}"
    info(clue)
    assert(!(t =:= expected), clue); ()
  }

  def assertChild(child: LightTypeTag, parent: LightTypeTag): Unit = {
    val clue = s"${child.repr} <?< ${parent.repr}"
    def failClue = s"1: ${child.debug()}\n2: ${parent.debug()}"
    info(clue)
    assert(child <:< parent, s"$clue\n$failClue"); ()
  }

  def assertNotChild(child: LightTypeTag, parent: LightTypeTag): Unit = {
    val clue = s"${child.repr} <!< ${parent.repr}"
    def failClue = s"1: ${child.debug()}\n2: ${parent.debug()}"
    info(clue)
    assert(!(child <:< parent), s"$clue\n$failClue"); ()
  }

  def assertCombine(outer: LightTypeTag, inner: Seq[LightTypeTag], expected: LightTypeTag): Unit = {
    val combined = outer.combine(inner: _*)
    val clue = s"(${outer.repr})•(${inner.map(_.repr).mkString(",")}) => ${combined.repr} =?= ${expected.repr}"
    info(clue)
    assert(combined =:= expected, clue); ()
  }

  def assertCombine(outer: LightTypeTag, inner: LightTypeTag, expected: LightTypeTag): Unit = {
    val combined = outer.combine(inner)
    val clue = s"(${outer.repr})•(${inner.repr}) => ${combined.repr} =?= ${expected.repr}"
    info(clue)
    assert(combined =:= expected, clue); ()
  }

  def assertCombineNonPos(outer: LightTypeTag, inner: Seq[Option[LightTypeTag]], expected: LightTypeTag): Unit = {
    val combined = outer.combineNonPos(inner: _*)
    val clue = s"(${outer.repr})•(${inner.map(_.map(_.repr)).mkString(",")}) => ${combined.repr} =?= ${expected.repr}"
    info(clue)
    assert(combined =:= expected, clue); ()
  }

  def assertIntersection(intersection: List[LightTypeTag], expected: LightTypeTag): Unit = {
    val intersected = LightTypeTag.refinedType(intersection, structure = LTT[Any], additionalTypeMembers = Map.empty)
    val clue = s"(${intersection.map(_.repr).mkString(" & ")}) => ${intersected.repr} =?= ${expected.repr}"
    info(clue)
    assert(intersected =:= expected, clue)
    assertDebugSame(intersected, expected)
    ()
  }

  def assertDeepSame(t: LightTypeTag, expected: LightTypeTag): Unit = {
    assertChild(t, expected)
    assertChild(expected, t)
    assertSame(t, expected)
    assertSameRef(t, expected)
  }

  def assertDeepChild(t: LightTypeTag, expected: LightTypeTag): Unit = {
    assertChild(t, expected)
    assertNotChild(expected, t)
    assertDifferent(expected, t)
  }

  def assertDeepNotChild(t: LightTypeTag, expected: LightTypeTag): Unit = {
    assertNotChild(t, expected)
    assertNotChild(expected, t)
    assertDifferent(expected, t)
  }

  def literalLtt(s: String)(implicit l: LTag[s.type]): LightTypeTag = l.tag

}
