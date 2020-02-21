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

import izumi.fundamentals.platform.cli.model.raw.{RawAppArgs, RawEntrypointParams, RawFlag, RawRoleParams, RawValue}
import org.scalatest.wordspec.AnyWordSpec

class CliParserTest extends AnyWordSpec {

  "CLI parser" should {
    "parse args" in {
      val v1 = RawAppArgs(RawEntrypointParams(Vector(RawFlag("help")), Vector(RawValue("x", "y"), RawValue("logs", "json"))), Vector(RawRoleParams("role1", RawEntrypointParams(Vector.empty, Vector(RawValue("config", "xxx"))), Vector("arg1", "arg2")), RawRoleParams("role2", RawEntrypointParams.empty, Vector.empty)))
      val v2= RawAppArgs(RawEntrypointParams(Vector(RawFlag("help")),Vector(RawValue("x","y"), RawValue("logs","json"))),Vector(RawRoleParams("role1",RawEntrypointParams(Vector.empty,Vector(RawValue("config","xxx"))),Vector("arg1", "arg2", "--yyy=zzz")), RawRoleParams("role2",RawEntrypointParams.empty,Vector.empty)))
      val v3 = RawAppArgs(RawEntrypointParams(Vector(RawFlag("help")),Vector(RawValue("x","y"), RawValue("logs","json"))),Vector(RawRoleParams("role1",RawEntrypointParams(Vector.empty,Vector.empty), Vector("--config=xxx", "arg1", "arg2", "--yyy=zzz")), RawRoleParams("role2",RawEntrypointParams.empty,Vector.empty)))
      val v4 = RawAppArgs(RawEntrypointParams(Vector(RawFlag("x"), RawFlag("x")),Vector(RawValue("x","y"))),Vector(RawRoleParams("role1",RawEntrypointParams(Vector(RawFlag("x"), RawFlag("x")),Vector(RawValue("x","y"), RawValue("xx","yy"))),Vector.empty)))
      assert(new CLIParser().parse(Array("--help", "--x=y", "--logs=json", ":role1", "--config=xxx", "arg1", "arg2", ":role2")) == Right(v1))
      assert(new CLIParser().parse(Array("--help", "--x=y", "--logs=json", ":role1", "--config=xxx", "arg1", "arg2", "--yyy=zzz", ":role2")) == Right(v2))
      assert(new CLIParser().parse(Array("--help", "--x=y", "--logs=json", ":role1", "--", "--config=xxx", "arg1", "arg2", "--yyy=zzz", ":role2")) == Right(v3))
      assert(new CLIParser().parse(Array("-x", "-x", "y", "-x", ":role1", "-x", "-x", "y", "-x", "--xx=yy")) == Right(v4))

      assert(new CLIParser().parse(Array("-x")).right.exists(_.globalParameters.flags.head.name == "x"))
      assert(new CLIParser().parse(Array("-x", "value")).right.exists(_.globalParameters.values.head == RawValue("x", "value")))
      assert(new CLIParser().parse(Array("--x", "value")).isLeft)
      assert(new CLIParser().parse(Array("--x=value")).right.exists(_.globalParameters.values.head == RawValue("x", "value")))
      assert(new CLIParser().parse(Array(":init", "./tmp")) == Right(RawAppArgs(RawEntrypointParams.empty,Vector(RawRoleParams("init",RawEntrypointParams.empty,Vector("./tmp"))))))
      assert(new CLIParser().parse(Array(":init", "--target=./tmp")) == Right(RawAppArgs(RawEntrypointParams.empty,Vector(RawRoleParams("init",RawEntrypointParams(Vector.empty,Vector(RawValue("target","./tmp"))),Vector.empty)))))
      assert(new CLIParser().parse(Array(":init", "-t", "./tmp")) == Right(RawAppArgs(RawEntrypointParams.empty,Vector(RawRoleParams("init",RawEntrypointParams(Vector.empty,Vector(RawValue("t","./tmp"))),Vector.empty)))))
    }
  }


}

