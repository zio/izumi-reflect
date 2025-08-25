package izumi.reflect

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Try

object ManualBenchmark {
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    println("=" * 30)
    println("  RUNNING MANUAL BENCHMARK")
    println("=" * 30)

    val tempDir = Files.createTempDirectory("izumi-manual-benchmark")
    val sourceFile = generateSource(tempDir)

    // --- Run without cache ---
    System.setProperty("izumi.reflect.cache.enabled", "false")
    println("\n[1] Compiling with cache DISABLED...")
    val disabledTimes = (1 to 6).map { i =>
      val start = System.nanoTime()
      compileOnce(sourceFile)
      val end = System.nanoTime()
      val durationMs = (end - start) / 1_000_000
      println(f"    Run $i: $durationMs%,7d ms")
      durationMs
    }

    // --- Run with cache ---
    System.clearProperty("izumi.reflect.cache.enabled")
    println("\n[2] Compiling with cache ENABLED...")
    val enabledTimes = (1 to 6).map { i =>
      val start = System.nanoTime()
      compileOnce(sourceFile)
      val end = System.nanoTime()
      val durationMs = (end - start) / 1_000_000
      println(f"    Run $i: $durationMs%,7d ms")
      durationMs
    }

    // --- Report Results ---
    // Drop first run as warmup
    val avgDisabled = disabledTimes.drop(1).sum / 5.0
    val avgEnabled = enabledTimes.drop(1).sum / 5.0
    val improvement = (1.0 - (avgEnabled / avgDisabled)) * 100

    println("\n" + "=" * 30)
    println("          RESULTS")
    println("=" * 30)
    println(f"  Avg. without cache: $avgDisabled%,.2f ms")
    println(f"  Avg. with cache:    $avgEnabled%,.2f ms")
    println("-" * 30)
    println(f"  Improvement:        $improvement%,.2f %%")
    println("=" * 30)

    deleteRecursively(tempDir.toFile)
  }

  def compileOnce(sourceFile: String): Unit = {
    val outputDir = Files.createTempDirectory("benchmark-output")
    val fullCp = sys.props.getOrElse(
      "full.classpath",
      throw new RuntimeException("full.classpath system property not set — make sure you added it in build.sbt")
    )

    val args = Array(
      "-d", outputDir.toAbsolutePath.toString,
      "-classpath", fullCp,
      sourceFile
    )

    val reporter = dotty.tools.dotc.Main.process(args)

    if (reporter.hasErrors) {
      throw new RuntimeException("Compilation failed")
    }
    deleteRecursively(outputDir.toFile)
  }

  def generateSource(srcDir: Path): String = {
    val sourceCode =
      """
        |package benchmark.test
        |import izumi.reflect.Tag
        |object Test {
        |  def t1 = Tag[Either[String, Either[Int, Boolean]]]
        |  def t2 = Tag[Either[String, Either[Int, Boolean]]]
        |  def t3 = Tag[Either[String, Either[Int, Boolean]]]
        |  def t4 = Tag[Either[String, Either[Int, Boolean]]]
        |  def t5 = Tag[Either[String, Either[Int, Boolean]]]
        |  def t6 = Tag[List[Map[String, Set[Vector[Int]]]]]
        |  def t7 = Tag[List[Map[String, Set[Vector[Int]]]]]
        |  def t8 = Tag[List[Map[String, Set[Vector[Int]]]]]
        |  def t9 = Tag[List[Map[String, Set[Vector[Int]]]]]
        |  def t10 = Tag[List[Map[String, Set[Vector[Int]]]]]
        |}
      """.stripMargin
    val outputFile = srcDir.resolve("TagBenchmarkCode.scala")
    Files.write(outputFile, sourceCode.getBytes("UTF-8"))
    outputFile.toAbsolutePath.toString
  }

  def deleteRecursively(file: File): Unit = {
    if (file.isDirectory) Option(file.listFiles()).foreach(_.foreach(deleteRecursively))
    file.delete()
  }
}