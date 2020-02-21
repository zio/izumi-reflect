
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

package izreflect.fundamentals.platform

import izreflect.fundamentals.platform.strings.IzString._
import org.scalatest.wordspec.AnyWordSpec

class IzStringTest extends AnyWordSpec {

  "Extended string" should {
    "support boolean parsing" in {
      assert("false".asBoolean().contains(false))
      assert("true".asBoolean().contains(true))

      assert("true".asBoolean(true))
      assert("x".asBoolean(true))
      assert(!"x".asBoolean(false))
      assert(!"false".asBoolean(false))

      assert(null.asInstanceOf[String].asBoolean().isEmpty)
      assert(null.asInstanceOf[String].asBoolean(true))
      assert(!null.asInstanceOf[String].asBoolean(false))
    }

    "support ellipsed leftpad" in {
      assert("x".leftEllipsed(5, "...") == "x")
      assert("xxxxxx".leftEllipsed(5, "...") == "...xx")
      assert("xx".leftEllipsed(1, "...") == "x")
    }

    "support ellipsed rightpad" in {
      assert("x".rightEllipsed(5, "...") == "x")
      assert("xxxxxx".rightEllipsed(5, "...") == "xx...")
      assert("xx".rightEllipsed(1, "...") == "x")
    }


    "support minimization" in {
      assert("x".minimize(0) == "x")
      assert("x.y.z".minimize(0) == "x.y.z")
      assert("x..z".minimize(0) == "x.z")
      assert("com.github.izreflect.Class".minimize(0) == "c.g.i.C")

      assert("x".minimize(1) == "x")
      assert("x.y.z".minimize(1) == "x.y.z")
      assert("x..z".minimize(1) == "x.z")
      assert("com.github.izreflect.Class".minimize(1) == "c.g.i.Class")
      assert("com.github.izreflect.Class".minimize(2) == "c.g.izreflect.Class")
    }

    "support splitFirst" in {
      assert("=1=2".splitFirst('=') == ("" -> "1=2"))
      assert("1=".splitFirst('=') == ("1" -> ""))
      assert("key=value=xxx".splitFirst('=') == ("key" -> "value=xxx"))
    }

    "support splitLast" in {
      assert("=1=2".splitLast('=') == ("=1" -> "2"))
      assert("1=".splitLast('=') == ("1" -> ""))
      assert("key=value=xxx".splitLast('=') == ("key=value" -> "xxx"))
    }

  }


}

