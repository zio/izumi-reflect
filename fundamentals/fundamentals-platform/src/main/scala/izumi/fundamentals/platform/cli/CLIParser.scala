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

package izumi.fundamentals.platform.cli

import izumi.fundamentals.platform.cli.CLIParser._
import izumi.fundamentals.platform.cli.impl.CLIParserState
import izumi.fundamentals.platform.cli.model.raw._

import scala.collection.mutable
import scala.language.implicitConversions

class CLIParser {
  implicit def eitherW[A, B](e: Either[A, B]) = new {
    def map[B1](f: B => B1): Either[A, B1] = e.right map f

    def flatMap[B1](f: B => Either[A, B1]): Either[A, B1] = e.right flatMap f
  }

  def parse(args: Array[String]): Either[ParserError, RawAppArgs] = {
    var state: CLIParserState = new CLIParserState.Initial()
    val processed = mutable.ArrayBuffer[String]()
    args.foreach {
      arg =>
        state = if (arg.startsWith(":") && arg.length > 1) {
          state.addRole(arg.substring(1))
        } else if (arg.startsWith("--") && arg.length > 2) {
          val argv = arg.substring(2)
          argv.indexOf('=') match {
            case -1 =>
              state.addFlag(arg)(RawFlag(argv))
            case pos =>
              val (k, v) = argv.splitAt(pos)
              state.addParameter(arg)(RawValue(k, v.substring(1)))
          }
        } else if (arg == "--") {
          state.splitter(processed.toVector)
        } else if (arg.startsWith("-")) {
          val argv = arg.substring(1)
          state.openParameter(arg)(argv)
        } else {
          state.addFreeArg(processed.toVector)(arg)
        }
        processed += arg
    }

    for {
      roles <- state.freeze()
      _ <- validate(roles)
    } yield roles
  }

  private[this] def validate(arguments: RawAppArgs): Either[ParserError, Unit] = {
    val bad = arguments.roles.groupBy(_.role).filter(_._2.size > 1)
    if (bad.nonEmpty) {
      Left(ParserError.DuplicatedRoles(bad.keySet))
    } else {
      Right(())
    }
  }
}

object CLIParser {

  sealed trait ParserError

  object ParserError {

    final case class DanglingArgument(processed: Vector[String], arg: String) extends ParserError

    final case class DanglingSplitter(processed: Vector[String]) extends ParserError

    final case class DuplicatedRoles(bad: Set[String]) extends ParserError

  }

}





