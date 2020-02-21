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

package izumi.fundamentals.platform.strings

object IzText {

  final case class Row(parts: Seq[String], splitter: String)

  def tableFormat(rows: Seq[Seq[String]], header: List[String]): String = {
    val splitChar = " | "

    import izumi.fundamentals.collections.IzCollections._
    val columnsCount = math.max(rows.map(_.size).maxOr(0), header.length)

    val bheader = header ++ List.fill(columnsCount - header.size)("")
    val bparts = rows.map(p => p ++ List.fill(columnsCount - p.size)(""))
    val withHeader = bheader +: bparts

    val maxes = (0 until columnsCount).map(c => c -> withHeader.map(_.apply(c).length).max)

    val maxesM = maxes.toMap

    val splitter = Row(maxes.map {
      case (_, len) =>
        "-" * len
    }, "-+-")

    val boundary = Row(maxes.map {
      case (_, len) =>
        "-" * len
    }, "---")


    val mainRows = bparts.map(p => Row(p, splitChar))

    (List(boundary) ++ List(Row(bheader, splitChar)) ++ List(splitter) ++ mainRows ++ List(boundary)).map {
      row =>
        row.parts.zipWithIndex.map {
          case (v, cnum) =>
            v.padTo(maxesM(cnum), ' ')
        }.mkString(row.splitter)
    }.mkString("\n")
  }
}
