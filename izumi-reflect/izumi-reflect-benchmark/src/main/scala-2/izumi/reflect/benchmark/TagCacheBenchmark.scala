package izumi.reflect.benchmark

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.util.Try

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.reporters.ConsoleReporter

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
  private var global: Global = _
  private var uniqueId: String = _

  @Setup(Level.Trial)
  def setupTrial(): Unit = {
    if (cacheEnabled == "false") {
      System.setProperty("izumi.reflect.rtti.cache.compile", "false")
    } else {
      System.clearProperty("izumi.reflect.rtti.cache.compile")
    }

    val benchmarkId = s"cache-${cacheEnabled}-miss-${cacheMissPercentage}-ops-${totalTagOperations}"
    tempDir = Files.createTempDirectory(s"izumi-reflect-benchmark-$benchmarkId")

    val actualCacheSetting = Option(System.getProperty("izumi.reflect.rtti.cache.compile")).getOrElse("default(true)")
    println(s"BENCHMARK: cache=$cacheEnabled, missRate=$cacheMissPercentage%, ops=$totalTagOperations, sysProp=$actualCacheSetting")
  }

  @Setup(Level.Iteration)
  def setupIteration(): Unit = {
    // Clear the global cache via reflection to ensure isolated iterations
    clearGlobalCache()

    if (cacheEnabled == "false") {
      System.setProperty("izumi.reflect.rtti.cache.compile", "false")
    } else {
      System.clearProperty("izumi.reflect.rtti.cache.compile")
    }
    uniqueId = System.nanoTime().toHexString
    sourceFiles = generateBenchmarkSources(tempDir, uniqueId)
    initializeCompiler()
  }

  @TearDown(Level.Trial)
  def tearDownTrial(): Unit = {
    Try {
      deleteRecursively(tempDir.toFile)
    }
  }

  private def initializeCompiler(): Unit = {
    val settings = new Settings()

    settings.classpath.value = findIzumiReflectClasspath()
    settings.usejavacp.value = true
    settings.nopredef.value = false
    settings.noimports.value = false

    // Configure cache via -Xmacro-settings which the Scala 2 macro checks in c.settings
    if (cacheEnabled == "false") {
      settings.XmacroSettings.value = List("izumi.reflect.rtti.cache.compile=false")
    } else {
      settings.XmacroSettings.value = List()
    }

    val reporter = new ConsoleReporter(settings) {
      override def displayPrompt(): Unit = ()
    }
    global = new Global(settings, reporter)
  }

  def compileImpl(blackhole: Blackhole): Unit = {
    val iterationId = System.nanoTime()
    val outputDir = new VirtualDirectory(s"output-$iterationId", None)

    global.reporter.reset()
    val compiler = global
    val run = new compiler.Run()

    val sources = sourceFiles.map { path =>
      val file = AbstractFile.getFile(path)
      file
    }

    Thread.sleep(1)

    run.compile(sources.map(_.path))

    if (compiler.reporter.hasErrors) {
      throw new RuntimeException(s"Compilation failed: cache=$cacheEnabled, miss=$cacheMissPercentage%")
    }

    blackhole.consume(run)
    blackhole.consume(outputDir.toString)
    blackhole.consume(sources.length)
    blackhole.consume(compiler.settings.classpath.value)
  }

  private def generateBenchmarkSources(srcDir: Path, uniqueId: String): List[String] = {
    val missCount = (totalTagOperations * cacheMissPercentage) / 100
    val hitCount = totalTagOperations - missCount

    // Generate realistic domain models that would use izumi-reflect
    val domainClasses = s"""
  // Realistic domain models like you'd find in a large codebase
  trait EventStore_${uniqueId}[F[_], Event]
  trait EventHandler_${uniqueId}[F[_], Event, Result]
  trait Repository_${uniqueId}[F[_], Entity, Id]
  trait Service_${uniqueId}[F[_], Request, Response]
  trait Cache_${uniqueId}[F[_], K, V]
  trait Metrics_${uniqueId}[F[_]]
  trait Logger_${uniqueId}[F[_]]

  case class UserId_${uniqueId}(value: String) extends AnyVal
  case class OrderId_${uniqueId}(value: String) extends AnyVal
  case class ProductId_${uniqueId}(value: String) extends AnyVal

  sealed trait UserEvent_${uniqueId}
  case class UserCreated_${uniqueId}(id: UserId_${uniqueId}, email: String, timestamp: Long) extends UserEvent_${uniqueId}
  case class UserUpdated_${uniqueId}(id: UserId_${uniqueId}, changes: Map[String, String], timestamp: Long) extends UserEvent_${uniqueId}
  case class UserDeleted_${uniqueId}(id: UserId_${uniqueId}, timestamp: Long) extends UserEvent_${uniqueId}

  case class User_${uniqueId}(id: UserId_${uniqueId}, email: String, profile: UserProfile_${uniqueId})
  case class UserProfile_${uniqueId}(firstName: String, lastName: String, preferences: Map[String, String])
  case class Order_${uniqueId}(id: OrderId_${uniqueId}, userId: UserId_${uniqueId}, status: OrderStatus_${uniqueId}, items: List[OrderItem_${uniqueId}])
  case class OrderItem_${uniqueId}(productId: ProductId_${uniqueId}, quantity: Int, price: BigDecimal)

  sealed trait OrderStatus_${uniqueId}
  case object Pending_${uniqueId} extends OrderStatus_${uniqueId}
  case object Processing_${uniqueId} extends OrderStatus_${uniqueId}
  case object Shipped_${uniqueId} extends OrderStatus_${uniqueId}"""

    // Generate unique types for cache misses
    val uniqueTypes = (0 until missCount).map { i =>
      s"""  case class UserEvent${i}_${uniqueId}(data: String) extends UserEvent_${uniqueId}
  case class CreateRequest${i}_${uniqueId}(email: String, profile: UserProfile_${uniqueId})"""
    }.mkString("\n")

    // Generate cache miss tags - simple unique types
    val cacheMissTagDefs = (0 until missCount).map { i =>
      s"  val missTag$i = Tag[UserEvent${i}_${uniqueId}]"
    }.mkString("\n")

    // Generate cache hit tags - simple repeated types
    val commonTypes = List(
      s"User_${uniqueId}",
      s"Order_${uniqueId}",
      s"UserEvent_${uniqueId}",
      s"OrderId_${uniqueId}"
    )

    val cacheHitTagDefs = (0 until hitCount).map { i =>
      val commonType = commonTypes(i % commonTypes.length)
      s"  val hitTag$i = Tag[$commonType]"
    }.mkString("\n")


    val sourceCode = s"""package izumi.reflect.benchmark.generated

import izumi.reflect.Tag
import scala.concurrent.Future
import scala.math.BigDecimal
import scala.util.Either

object TagBenchmarkCode {""" + "\n" +
      domainClasses + "\n\n" +
      uniqueTypes + "\n\n" +
      cacheMissTagDefs + "\n\n" +
      cacheHitTagDefs + "\n\n" +
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
      "}"

    val outputFile = srcDir.resolve(s"TagBenchmarkCode_${uniqueId}.scala")
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
      // Access the globalCache field directly from LightTypeTagImpl object
      val lightTypeTagImplClass = Class.forName("izumi.reflect.macrortti.LightTypeTagImpl$")
      val moduleField = lightTypeTagImplClass.getField("MODULE$")
      val moduleInstance = moduleField.get(null)

      // Get the globalCache field
      val cacheField = lightTypeTagImplClass.getDeclaredField("globalCache")
      cacheField.setAccessible(true)
      val cache = cacheField.get(moduleInstance).asInstanceOf[java.util.concurrent.ConcurrentHashMap[Any, Any]]

      val sizeBefore = cache.size()
      cache.clear()
      val sizeAfter = cache.size()

      println(s"CACHE: Cleared $sizeBefore entries, now $sizeAfter entries")
    } catch {
      case e: Exception =>
        // Also try clearing the Scala 3 cache in case we're in mixed mode
        try {
          val inspectClass = Class.forName("izumi.reflect.dottyreflection.Inspect$")
          val moduleField = inspectClass.getField("MODULE$")
          val moduleInstance = moduleField.get(null)
          val cacheField = inspectClass.getDeclaredField("compilationCache")
          cacheField.setAccessible(true)
          val cache = cacheField.get(moduleInstance).asInstanceOf[java.util.concurrent.ConcurrentHashMap[Any, Any]]
          val sizeBefore = cache.size()
          cache.clear()
          println(s"CACHE: Cleared Scala 3 cache: $sizeBefore entries")
        } catch {
          case e2: Exception =>
            println(s"CACHE: Failed to clear caches: Scala 2: ${e.getMessage}, Scala 3: ${e2.getMessage}")
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
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G", "-Xss8M"))
class ColdTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G", "-Xss8M"))
class WarmTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G", "-Xss8M"))
class HotTagCacheBenchmark extends BaseTagCacheBenchmark {
  @Benchmark
  def compile(blackhole: Blackhole): Unit = compileImpl(blackhole)
}