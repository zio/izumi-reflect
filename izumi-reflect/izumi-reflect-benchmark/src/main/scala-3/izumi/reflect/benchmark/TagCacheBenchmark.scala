package izumi.reflect.benchmark

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.util.Try

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.core.Contexts._

import izumi.reflect.Tag

@State(Scope.Benchmark)
class BaseTagCacheBenchmark {

  @Param(Array("5", "10", "20"))
  var cacheMissPercentage: Int = _

  @Param(Array("50"))
  var totalTagOperations: Int = _

  @Param(Array("true", "false"))
  var cacheEnabled: String = _

  private var tempDir: Path = _
  private var sourceFiles: List[String] = _

  @Setup(Level.Trial)
  def setupTrial(): Unit = {
    // Clear any existing cache state
    System.gc()

    if (cacheEnabled == "false") {
      System.setProperty("izumi.reflect.rtti.cache.compile", "false")
    } else {
      System.clearProperty("izumi.reflect.rtti.cache.compile")
    }

    val benchmarkId = s"cache-${cacheEnabled}-miss-${cacheMissPercentage}-ops-${totalTagOperations}-${System.nanoTime()}"
    tempDir = Files.createTempDirectory(s"izumi-reflect-benchmark-$benchmarkId")

    sourceFiles = generateBenchmarkSources(tempDir)

    val actualCacheSetting = Option(System.getProperty("izumi.reflect.rtti.cache.compile")).getOrElse("default(true)")
    println(s"BENCHMARK: cache=$cacheEnabled, missRate=$cacheMissPercentage%, ops=$totalTagOperations, sysProp=$actualCacheSetting")
  }

  @Setup(Level.Iteration)
  def setupIteration(): Unit = {
    // Clear the global cache via reflection to ensure isolated iterations
    clearGlobalCache()

    // Keep same cache configuration for all iterations
    if (cacheEnabled == "false") {
      System.setProperty("izumi.reflect.rtti.cache.compile", "false")
    } else {
      System.clearProperty("izumi.reflect.rtti.cache.compile")
    }

    // DON'T generate new source files - reuse same files to test cache effectiveness!
    // The cache should speed up compilation of the SAME types across multiple runs
  }

  @TearDown(Level.Trial)
  def tearDownTrial(): Unit = {
    Try {
      deleteRecursively(tempDir.toFile)
    }
  }

  def compileImpl(blackhole: Blackhole): Unit = {
    if (cacheEnabled == "false") {
      System.setProperty("izumi.reflect.rtti.cache.compile", "false")
    } else {
      System.clearProperty("izumi.reflect.rtti.cache.compile")
    }

    val iterationId = System.nanoTime()
    val outputDir = tempDir.resolve(s"output-$iterationId")
    if (Files.exists(outputDir)) {
      deleteRecursively(outputDir.toFile)
    }
    Files.createDirectories(outputDir)

    val dummyFile = outputDir.resolve("dummy.tmp")
    Files.write(dummyFile, "warmup".getBytes())
    Files.delete(dummyFile)

    implicit val ctx = new ContextBase().initialCtx.fresh

    ctx.setSetting(ctx.settings.usejavacp, true)
    ctx.setSetting(ctx.settings.YdropComments, true)
    ctx.setSetting(ctx.settings.silentWarnings, true)
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.Vprofile, false)

    val classpath = findIzumiReflectClasspath()
    if (classpath.nonEmpty) {
      ctx.setSetting(ctx.settings.classpath, classpath)
    }

    ctx.setSetting(ctx.settings.outputDir, dotty.tools.io.AbstractFile.getDirectory(outputDir.toAbsolutePath.toString))

    val compiler = new dotty.tools.dotc.Compiler

    val abstractFiles = sourceFiles.map { path =>
      dotty.tools.io.AbstractFile.getFile(path)
    }

    Thread.sleep(1)

    val run = compiler.newRun
    run.compile(abstractFiles)
    val reporter = ctx.reporter

    if (reporter.hasErrors) {
      val errors = reporter.allErrors.map(_.message).mkString("; ")
      throw new RuntimeException(s"Compilation failed: cache=$cacheEnabled, miss=$cacheMissPercentage%, errors=$errors")
    }

    blackhole.consume(reporter)
    blackhole.consume(outputDir.toString)
    blackhole.consume(sourceFiles.length)
    blackhole.consume(classpath)
  }

  private def generateBenchmarkSources(srcDir: Path, iterationId: Option[Long] = None): List[String] = {
    val missCount = (totalTagOperations * cacheMissPercentage) / 100
    val hitCount = totalTagOperations - missCount

    // Generate realistic domain models that would use izumi-reflect
    val domainClasses = """
  // Realistic domain models like you'd find in a large codebase
  trait EventStore[F[_], Event]
  trait EventHandler[F[_], Event, Result]
  trait Repository[F[_], Entity, Id]
  trait Service[F[_], Request, Response]
  trait Cache[F[_], K, V]
  trait Metrics[F[_]]
  trait Logger[F[_]]

  case class UserId(value: String) extends AnyVal
  case class OrderId(value: String) extends AnyVal
  case class ProductId(value: String) extends AnyVal

  sealed trait UserEvent
  case class UserCreated(id: UserId, email: String, timestamp: Long) extends UserEvent
  case class UserUpdated(id: UserId, changes: Map[String, Any], timestamp: Long) extends UserEvent
  case class UserDeleted(id: UserId, timestamp: Long) extends UserEvent

  case class User(id: UserId, email: String, profile: UserProfile)
  case class UserProfile(firstName: String, lastName: String, preferences: Map[String, String])
  case class Order(id: OrderId, userId: UserId, status: OrderStatus, items: List[OrderItem])
  case class OrderItem(productId: ProductId, quantity: Int, price: BigDecimal)

  sealed trait OrderStatus
  case object Pending extends OrderStatus
  case object Processing extends OrderStatus
  case object Shipped extends OrderStatus"""

    // Generate unique types for cache misses
    val uniqueTypes = (0 until missCount).map { i =>
      s"""  case class UserEvent$i(data: String) extends UserEvent
  case class CreateRequest$i(email: String, profile: UserProfile)"""
    }.mkString("\n")

    // Generate cache miss tags - simple unique types
    val cacheMissTagDefs = (0 until missCount).map { i =>
      s"  val missTag$i = Tag[UserEvent$i]"
    }.mkString("\n")

    // Generate cache hit tags - simple repeated types
    val commonTypes = List(
      "User",
      "Order",
      "UserEvent",
      "OrderId"
    )

    val cacheHitTagDefs = (0 until hitCount).map { i =>
      val commonType = commonTypes(i % commonTypes.length)
      s"  val hitTag$i = Tag[$commonType]"
    }.mkString("\n")

    // Generate cache-friendly tag patterns for enterprise codebases
    val manyTagsUsage = s"""
  object ServiceRegistry {
    // Shared repository types (high cache reuse)
    ${(0 until hitCount / 2).map(i => s"val userRepo$i = Tag[Repository[scala.concurrent.Future, User, UserId]]").mkString("\n    ")}

    // Shared service types (high cache reuse)
    ${(0 until hitCount / 2).map(i => s"val userService$i = Tag[Service[scala.concurrent.Future, User, Either[String, User]]]").mkString("\n    ")}

    // Force tag evaluation
    val allTags = List(
      ${(0 until hitCount / 2).map(i => s"userRepo$i.tag").mkString(", ")},
      ${(0 until hitCount / 2).map(i => s"userService$i.tag").mkString(", ")}
    )
  }"""

    val packageSuffix = iterationId.map(id => s"_$id").getOrElse("")
    val sourceCode = s"package izumi.reflect.benchmark.generated$packageSuffix" + "\n\n" +
      "import izumi.reflect.Tag" + "\n\n" +
      "object TagBenchmarkCode {" + "\n" +
      domainClasses + "\n\n" +
      uniqueTypes + "\n\n" +
      cacheMissTagDefs + "\n\n" +
      cacheHitTagDefs + "\n\n" +
      manyTagsUsage + "\n\n" +
      "  val allTags = Seq(" + "\n" +
      (0 until missCount).map(i => s"    missTag$i").mkString(",\n") +
      (if (missCount > 0 && hitCount > 0) ",\n" else "\n") +
      (0 until hitCount).map(i => s"    hitTag$i").mkString(",\n") + "\n" +
      "  )" + "\n\n" +
      "  val tagData = allTags.map(_.tag.longNameWithPrefix)" + "\n" +
      "  val tagUsage = tagData.zipWithIndex.map { case (name, idx) =>" + "\n" +
      "    s\"${idx}_${name.hashCode}\"" + "\n" +
      "  }.mkString(\",\")" + "\n\n" +
      "  val tagCount = allTags.length" + "\n" +
      "  val tagSample = if (allTags.nonEmpty) allTags.head.tag.longNameWithPrefix else \"none\"" + "\n" +
      "  \n  // Force evaluation of the ServiceRegistry to trigger tag computation" + "\n" +
      "  val registryData = ServiceRegistry.allTags.map(_.longNameWithPrefix)" + "\n" +
      "}"

    val fileName = iterationId.map(id => s"TagBenchmarkCode_$id.scala").getOrElse("TagBenchmarkCode.scala")
    val outputFile = srcDir.resolve(fileName)
    Files.write(outputFile, sourceCode.getBytes("UTF-8"))

    List(outputFile.toAbsolutePath.toString)
  }

  private def findIzumiReflectClasspath(): String = {
    val classpath = System.getProperty("java.class.path")
    val classpathEntries = classpath.split(File.pathSeparator)

    val izumiReflectJars = classpathEntries.filter(entry =>
      entry.contains("izumi-reflect") && entry.endsWith(".jar")
    )

    if (izumiReflectJars.isEmpty) {
      classpath
    } else {
      izumiReflectJars.mkString(File.pathSeparator)
    }
  }

  private def clearGlobalCache(): Unit = {
    try {
      // Clear the Scala 3 compilation cache first
      val inspectClass = Class.forName("izumi.reflect.dottyreflection.Inspect$")
      val moduleField = inspectClass.getField("MODULE$")
      val moduleInstance = moduleField.get(null)
      val cacheField = inspectClass.getDeclaredField("compilationCache")
      cacheField.setAccessible(true)
      val cache = cacheField.get(moduleInstance).asInstanceOf[java.util.concurrent.ConcurrentHashMap[Any, Any]]

      val sizeBefore = cache.size()
      cache.clear()
      val sizeAfter = cache.size()

      println(s"CACHE: Cleared Scala 3 cache: $sizeBefore entries, now $sizeAfter entries")
    } catch {
      case e: Exception =>
        // Also try clearing the Scala 2 cache in case we're in mixed mode
        try {
          val lightTypeTagImplClass = Class.forName("izumi.reflect.macrortti.LightTypeTagImpl$")
          val moduleField = lightTypeTagImplClass.getField("MODULE$")
          val moduleInstance = moduleField.get(null)
          val cacheField = lightTypeTagImplClass.getDeclaredField("globalCache")
          cacheField.setAccessible(true)
          val cache = cacheField.get(moduleInstance).asInstanceOf[java.util.concurrent.ConcurrentHashMap[Any, Any]]
          val sizeBefore = cache.size()
          cache.clear()
          println(s"CACHE: Cleared Scala 2 cache: $sizeBefore entries")
        } catch {
          case e2: Exception =>
            println(s"CACHE: Failed to clear caches: Scala 3: ${e.getMessage}, Scala 2: ${e2.getMessage}")
        }
    }
  }

  private def deleteRecursively(file: File): Unit = {
    if (file.exists()) {
      if (file.isDirectory) {
        Option(file.listFiles()).foreach(_.foreach(deleteRecursively))
      }
      file.delete()
    }
  }
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 0)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-XX:CICompilerCount=2", "-Xms2G", "-Xmx2G", "-Xss2M"))
class ColdTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G", "-Xss8M"))
class WarmTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G", "-Xss8M"))
class HotTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}