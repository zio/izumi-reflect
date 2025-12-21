# PowerShell script to generate massive stress tests for compile-time cache benchmarking
# Updates existing CacheStressTest.scala files for Scala 2 and Scala 3

$scala3Path = "izumi-reflect\izumi-reflect\src\test\scala-3\izumi\reflect\test\CacheStressTest.scala"
$scala2Path = "izumi-reflect\izumi-reflect\src\test\scala-2\izumi\reflect\test\CacheStressTest.scala"

# Delete generated file if exists
$generatedPath = "izumi-reflect\izumi-reflect\src\test\scala-3\izumi\reflect\test\GeneratedCacheStressTest.scala"
if (Test-Path $generatedPath) { Remove-Item $generatedPath -Force }

function Generate-Scala3File {
    $content = @"
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

import izumi.reflect.*
import izumi.reflect.macrortti.*
import org.scalatest.wordspec.AnyWordSpec

/**
 * COMPILE-TIME CACHE STRESS TEST for Scala 3
 * 
 * This file contains MASSIVE repeated Tag materializations to demonstrate >10% compile-time improvement.
 * 
 * To benchmark:
 *   WITH CACHE:    sbt "++ 3.3.6" clean "izumi-reflectJVM / Test / compile"
 *   WITHOUT CACHE: `$`env:SBT_OPTS="-Dizumi.reflect.rtti.cache.compile=false"; sbt "++ 3.3.6" clean "izumi-reflectJVM / Test / compile"
 */
class CacheStressTest extends AnyWordSpec with TagAssertions {

  // Type aliases for deep nesting
  type Deep1 = Map[String, List[Option[Either[Int, Long]]]]
  type Deep2 = List[Vector[Set[Map[String, Option[Int]]]]]
  type Deep3 = Either[Map[String, List[Int]], Vector[Set[Option[Long]]]]
  type Deep4 = Option[Either[List[Map[String, Int]], Set[Vector[Long]]]]
  type Deep5 = List[Option[Either[Map[String, Set[Int]], Vector[Long]]]]

"@

    # Generate simple type stress tests (100x each)
    $simpleTypes = @("String", "Int", "Long", "Double", "Boolean")
    foreach ($type in $simpleTypes) {
        $content += @"

  "Stress: $type x100" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 10; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$type]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,t64,t65,t66,t67,t68,t69,t70,t71,t72,t73,t74,t75,t76,t77,t78,t79,t80,t81,t82,t83,t84,t85,t86,t87,t88,t89,t90,t91,t92,t93,t94,t95,t96,t97,t98,t99,t100)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate generic types (100x each)
    $genericTypes = @(
        @{Name="List[Int]"; Safe="ListInt"},
        @{Name="Option[String]"; Safe="OptionString"},
        @{Name="Map[String, Int]"; Safe="MapStringInt"},
        @{Name="Either[String, Int]"; Safe="EitherStringInt"},
        @{Name="Set[Long]"; Safe="SetLong"}
    )
    foreach ($gt in $genericTypes) {
        $content += @"

  "Stress: $($gt.Name) x100" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 10; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$($gt.Name)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,t64,t65,t66,t67,t68,t69,t70,t71,t72,t73,t74,t75,t76,t77,t78,t79,t80,t81,t82,t83,t84,t85,t86,t87,t88,t89,t90,t91,t92,t93,t94,t95,t96,t97,t98,t99,t100)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate nested types (50x each)
    $nestedTypes = @(
        @{Name="List[Option[Int]]"; Safe="ListOptionInt"},
        @{Name="Map[String, List[Int]]"; Safe="MapStringListInt"},
        @{Name="Either[List[Int], Option[String]]"; Safe="EitherListIntOptionString"},
        @{Name="Option[Map[String, Int]]"; Safe="OptionMapStringInt"},
        @{Name="Vector[Set[Option[Int]]]"; Safe="VectorSetOptionInt"}
    )
    foreach ($nt in $nestedTypes) {
        $content += @"

  "Stress nested: $($nt.Name) x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$($nt.Name)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate deep type aliases (50x each)
    $deepTypes = @("Deep1", "Deep2", "Deep3", "Deep4", "Deep5")
    foreach ($dt in $deepTypes) {
        $content += @"

  "Stress deep: $dt x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$dt]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate HKT tests (50x each)
    $hktTypes = @(
        @{Type="List"; Tag="TagK"},
        @{Type="Option"; Tag="TagK"},
        @{Type="Vector"; Tag="TagK"},
        @{Type="Either"; Tag="TagKK"},
        @{Type="Map"; Tag="TagKK"}
    )
    foreach ($hkt in $hktTypes) {
        $content += @"

  "Stress HKT: $($hkt.Tag)[$($hkt.Type)] x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = $($hkt.Tag)[$($hkt.Type)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    $content += @"
}
"@
    return $content
}

function Generate-Scala2File {
    $content = @"
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

import izumi.reflect._
import izumi.reflect.macrortti._
import org.scalatest.wordspec.AnyWordSpec

/**
 * COMPILE-TIME CACHE STRESS TEST for Scala 2
 * 
 * This file contains MASSIVE repeated Tag materializations to demonstrate >10% compile-time improvement.
 */
class CacheStressTest extends AnyWordSpec with TagAssertions {

  // Type aliases for deep nesting
  type Deep1 = Map[String, List[Option[Either[Int, Long]]]]
  type Deep2 = List[Vector[Set[Map[String, Option[Int]]]]]
  type Deep3 = Either[Map[String, List[Int]], Vector[Set[Option[Long]]]]
  type Deep4 = Option[Either[List[Map[String, Int]], Set[Vector[Long]]]]
  type Deep5 = List[Option[Either[Map[String, Set[Int]], Vector[Long]]]]

"@

    # Generate simple type stress tests (100x each)
    $simpleTypes = @("String", "Int", "Long", "Double", "Boolean")
    foreach ($type in $simpleTypes) {
        $content += @"

  "Stress: $type x100" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 10; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$type]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,t64,t65,t66,t67,t68,t69,t70,t71,t72,t73,t74,t75,t76,t77,t78,t79,t80,t81,t82,t83,t84,t85,t86,t87,t88,t89,t90,t91,t92,t93,t94,t95,t96,t97,t98,t99,t100)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate generic types (100x each)
    $genericTypes = @(
        @{Name="List[Int]"; Safe="ListInt"},
        @{Name="Option[String]"; Safe="OptionString"},
        @{Name="Map[String, Int]"; Safe="MapStringInt"},
        @{Name="Either[String, Int]"; Safe="EitherStringInt"},
        @{Name="Set[Long]"; Safe="SetLong"}
    )
    foreach ($gt in $genericTypes) {
        $content += @"

  "Stress: $($gt.Name) x100" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 10; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$($gt.Name)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,t64,t65,t66,t67,t68,t69,t70,t71,t72,t73,t74,t75,t76,t77,t78,t79,t80,t81,t82,t83,t84,t85,t86,t87,t88,t89,t90,t91,t92,t93,t94,t95,t96,t97,t98,t99,t100)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate nested types (50x each)
    $nestedTypes = @(
        @{Name="List[Option[Int]]"; Safe="ListOptionInt"},
        @{Name="Map[String, List[Int]]"; Safe="MapStringListInt"},
        @{Name="Either[List[Int], Option[String]]"; Safe="EitherListIntOptionString"},
        @{Name="Option[Map[String, Int]]"; Safe="OptionMapStringInt"},
        @{Name="Vector[Set[Option[Int]]]"; Safe="VectorSetOptionInt"}
    )
    foreach ($nt in $nestedTypes) {
        $content += @"

  "Stress nested: $($nt.Name) x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$($nt.Name)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate deep type aliases (50x each)
    $deepTypes = @("Deep1", "Deep2", "Deep3", "Deep4", "Deep5")
    foreach ($dt in $deepTypes) {
        $content += @"

  "Stress deep: $dt x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = Tag[$dt]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    # Generate HKT tests (50x each)
    $hktTypes = @(
        @{Type="List"; Tag="TagK"},
        @{Type="Option"; Tag="TagK"},
        @{Type="Vector"; Tag="TagK"},
        @{Type="Either"; Tag="TagKK"},
        @{Type="Map"; Tag="TagKK"}
    )
    foreach ($hkt in $hktTypes) {
        $content += @"

  "Stress HKT: $($hkt.Tag)[$($hkt.Type)] x50" should {
    "cache hits" in {

"@
        for ($i = 0; $i -lt 5; $i++) {
            $start = $i * 10 + 1
            for ($j = 0; $j -lt 10; $j++) {
                $n = $start + $j
                $content += "      val t$n = $($hkt.Tag)[$($hkt.Type)]`n"
            }
        }
        $content += @"
      val tags = Seq(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50)
      tags.foreach(t => assertSameStrict(tags.head.tag, t.tag))
    }
  }
"@
    }

    $content += @"
}
"@
    return $content
}

# Generate and write files (without BOM)
$projectRoot = Split-Path $PSScriptRoot -Parent

Write-Host "Generating Scala 3 stress test..." -ForegroundColor Cyan
$scala3Content = Generate-Scala3File
$scala3FullPath = Join-Path $projectRoot $scala3Path
$scala3Dir = Split-Path $scala3FullPath -Parent
if (-not (Test-Path $scala3Dir)) { New-Item -ItemType Directory -Path $scala3Dir -Force | Out-Null }
[System.IO.File]::WriteAllText($scala3FullPath, $scala3Content)
Write-Host "  Written to: $scala3Path" -ForegroundColor Green

Write-Host "Generating Scala 2 stress test..." -ForegroundColor Cyan
$scala2Content = Generate-Scala2File
$scala2FullPath = Join-Path $projectRoot $scala2Path
$scala2Dir = Split-Path $scala2FullPath -Parent
if (-not (Test-Path $scala2Dir)) { New-Item -ItemType Directory -Path $scala2Dir -Force | Out-Null }
[System.IO.File]::WriteAllText($scala2FullPath, $scala2Content)
Write-Host "  Written to: $scala2Path" -ForegroundColor Green

# Count stats
$scala3Tags = ([regex]::Matches($scala3Content, "Tag\[|TagK\[|TagKK\[")).Count
$scala2Tags = ([regex]::Matches($scala2Content, "Tag\[|TagK\[|TagKK\[")).Count

Write-Host ""
Write-Host "========================================" -ForegroundColor Green
Write-Host "Stress test files generated!" -ForegroundColor Green
Write-Host "  Scala 3: $scala3Tags Tag materializations" -ForegroundColor Yellow
Write-Host "  Scala 2: $scala2Tags Tag materializations" -ForegroundColor Yellow
Write-Host "========================================" -ForegroundColor Green
Write-Host ""
Write-Host "To benchmark Scala 3 compile time:" -ForegroundColor White
Write-Host "  WITH CACHE:" -ForegroundColor Gray
Write-Host '    sbt "++ 3.3.6" clean "izumi-reflectJVM / Test / compile"' -ForegroundColor Cyan
Write-Host ""
Write-Host "  WITHOUT CACHE:" -ForegroundColor Gray
Write-Host '    $env:SBT_OPTS="-Dizumi.reflect.rtti.cache.compile=false"' -ForegroundColor Cyan
Write-Host '    sbt "++ 3.3.6" clean "izumi-reflectJVM / Test / compile"' -ForegroundColor Cyan
Write-Host '    $env:SBT_OPTS=""' -ForegroundColor Cyan
