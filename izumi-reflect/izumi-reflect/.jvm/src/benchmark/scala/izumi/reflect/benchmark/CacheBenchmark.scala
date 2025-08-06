package izumi.reflect.benchmark

import izumi.reflect.{DebugProperties, Tag}
import izumi.reflect.macrortti.LTag
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 2, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms512m", "-Xmx512m", "-XX:+UseG1GC"))
class CacheBenchmark {

  @Param(Array("true", "false"))
  var cacheEnabled: String = _

  private var originalCacheProperty: String = _

  @Setup(Level.Trial)
  def setupTrial(): Unit = {
    originalCacheProperty = System.getProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`)
    System.setProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`, cacheEnabled)

    // Pre-warm the JVM and Tag caches (lightweight)
    (1 to 100).foreach { _ =>
      Tag[String].tag
      Tag[Int].tag
      LTag[List[String]].tag
    }

    System.gc()
    Thread.sleep(50)
  }

  @TearDown(Level.Trial)
  def tearDownTrial(): Unit = {
    if (originalCacheProperty != null) {
      System.setProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`, originalCacheProperty)
    } else {
      System.clearProperty(DebugProperties.`izumi.reflect.rtti.cache.compile`)
    }
  }

  @Benchmark
  def simpleTagCreation(bh: Blackhole): Unit = {
    // Test full Tag creation (not just .tag access)
    bh.consume(Tag[String])
    bh.consume(Tag[Int])
    bh.consume(Tag[Long])
    bh.consume(Tag[Double])
    bh.consume(Tag[Boolean])
  }
  
  @Benchmark
  def simpleTagAccess(bh: Blackhole): Unit = {
    // Test tag access from cached Tags
    bh.consume(Tag[String].tag)
    bh.consume(Tag[Int].tag)
    bh.consume(Tag[Long].tag)
    bh.consume(Tag[Double].tag)
    bh.consume(Tag[Boolean].tag)
  }

  @Benchmark
  def complexTagCreation(bh: Blackhole): Unit = {
    // Test full Tag creation for complex types
    bh.consume(Tag[List[String]])
    bh.consume(Tag[Map[String, Int]])
    bh.consume(Tag[Either[String, Int]])
    bh.consume(Tag[Option[List[String]]])
    bh.consume(Tag[Vector[Map[String, Option[Int]]]])
  }
  
  @Benchmark
  def complexTagAccess(bh: Blackhole): Unit = {
    // Test tag access from cached complex Tags
    bh.consume(Tag[List[String]].tag)
    bh.consume(Tag[Map[String, Int]].tag)
    bh.consume(Tag[Either[String, Int]].tag)
    bh.consume(Tag[Option[List[String]]].tag)
    bh.consume(Tag[Vector[Map[String, Option[Int]]]].tag)
  }

  @Benchmark
  def deeplyNestedTagCreation(bh: Blackhole): Unit = {
    // Test full Tag creation for deeply nested types
    bh.consume(Tag[Map[String, List[Either[Throwable, Option[Int]]]]])
    bh.consume(Tag[List[Map[String, Either[Exception, Option[Long]]]]])
    bh.consume(Tag[Either[List[String], Map[Int, Option[Double]]]])
  }
  
  @Benchmark
  def deeplyNestedTagAccess(bh: Blackhole): Unit = {
    // Test tag access from cached deeply nested Tags
    bh.consume(Tag[Map[String, List[Either[Throwable, Option[Int]]]]].tag)
    bh.consume(Tag[List[Map[String, Either[Exception, Option[Long]]]]].tag)
    bh.consume(Tag[Either[List[String], Map[Int, Option[Double]]]].tag)
  }

  @Benchmark
  def hierarchyTagCreation(bh: Blackhole): Unit = {
    // Define test types inline to avoid test dependencies
    trait TestI1
    trait TestI2 extends TestI1

    bh.consume(Tag[String].tag)
    bh.consume(Tag[TestI2].tag)
    bh.consume(Tag[TestI1].tag)
    bh.consume(Tag[AnyRef].tag)
    bh.consume(Tag[Any].tag)
    bh.consume(Tag[Object].tag)
  }

  @Benchmark
  def ltagCreation(bh: Blackhole): Unit = {
    // Test LTag creation (lightweight reference test)
    bh.consume(LTag[String])
    bh.consume(LTag[List[String]])
    bh.consume(LTag[Map[String, Int]])
    bh.consume(LTag[Either[String, Int]])
  }

  // Benchmarks addressing TODO: benchmark difference between searching all arguments vs. merge strategy
  
  @Benchmark
  def allArgumentsSearchStrategy(bh: Blackhole): Unit = {
    // Test Tag creation for types that require searching all type arguments
    bh.consume(Tag[Map[String, List[Either[Int, String]]]])
    bh.consume(Tag[Either[List[String], Map[Int, Option[Double]]]])
    bh.consume(Tag[List[Map[String, Either[Exception, Option[Long]]]]])
    bh.consume(Tag[Option[Either[List[String], Map[String, Int]]]])
  }

  @Benchmark
  def mergeStrategySearch(bh: Blackhole): Unit = {
    // Test Tag creation using merge strategy (simpler type combinations)
    bh.consume(Tag[List[String]])
    bh.consume(Tag[Map[String, Int]])
    bh.consume(Tag[Either[String, Int]])
    bh.consume(Tag[Option[String]])
    bh.consume(Tag[Vector[Int]])
  }

  @Benchmark
  def recursiveTypeResolution(bh: Blackhole): Unit = {
    // Test complex recursive type parameter resolution
    bh.consume(Tag[Map[List[String], Either[Option[Int], Vector[Double]]]])
    bh.consume(Tag[Either[Map[String, List[Int]], Option[Vector[String]]]])
    bh.consume(Tag[List[Either[Map[String, Int], Option[Vector[Boolean]]]]])
  }

  @Benchmark
  def tagComparison(bh: Blackhole): Unit = {
    val stringTag = Tag[String]
    val intTag = Tag[Int]
    val listStringTag = Tag[List[String]]
    val mapStringIntTag = Tag[Map[String, Int]]

    bh.consume(stringTag.tag =:= intTag.tag)
    bh.consume(stringTag.tag =:= stringTag.tag)
    bh.consume(listStringTag.tag =:= mapStringIntTag.tag)
  }

  @Benchmark
  def subtypeChecking(bh: Blackhole): Unit = {
    val childTags = Array(Tag[String], Tag[List[String]])
    val parentTags = Array(Tag[Any], Tag[AnyRef])

    var i = 0
    while (i < childTags.length) {
      var j = 0
      while (j < parentTags.length) {
        bh.consume(childTags(i).tag <:< parentTags(j).tag)
        j += 1
      }
      i += 1
    }
  }

  @Benchmark
  def tagCreationPatternComparison(bh: Blackhole): Unit = {
    // Direct comparison of all three patterns on the same types
    // ProviderMagnet pattern
    bh.consume(implicitly[Tag[String]])
    // Identity macro pattern  
    bh.consume(Tag.apply[String])
    // Direct construction pattern
    bh.consume(Tag(classOf[String], LTag[String].tag))
    
    // Repeat for complex type
    bh.consume(implicitly[Tag[List[String]]])
    bh.consume(Tag.apply[List[String]])
    bh.consume(Tag(classOf[scala.collection.immutable.List[_]], LTag[List[String]].tag))
  }

  // Benchmarks addressing TODO: benchmark ProviderMagnet vs. identity macro vs. normal function

  @Benchmark
  def providerMagnetPattern(bh: Blackhole): Unit = {
    // Test implicit Tag summoning (ProviderMagnet pattern - uses implicit resolution)
    bh.consume(implicitly[Tag[String]])
    bh.consume(implicitly[Tag[List[String]]])
    bh.consume(implicitly[Tag[Map[String, Int]]])
    bh.consume(implicitly[Tag[Either[String, Int]]])
    bh.consume(implicitly[Tag[Option[List[String]]]])
  }

  @Benchmark
  def identityMacroPattern(bh: Blackhole): Unit = {
    // Test direct macro invocation (identity macro pattern)
    bh.consume(Tag.apply[String])
    bh.consume(Tag.apply[List[String]])
    bh.consume(Tag.apply[Map[String, Int]])
    bh.consume(Tag.apply[Either[String, Int]])
    bh.consume(Tag.apply[Option[List[String]]])
  }

  @Benchmark
  def normalFunctionPattern(bh: Blackhole): Unit = {
    // Test normal function calls (bypassing macros where possible)
    val stringTag = Tag(classOf[String], LTag[String].tag)
    val intTag = Tag(classOf[java.lang.Integer], LTag[Int].tag)
    val booleanTag = Tag(classOf[java.lang.Boolean], LTag[Boolean].tag)
    val longTag = Tag(classOf[java.lang.Long], LTag[Long].tag)
    val doubleTag = Tag(classOf[java.lang.Double], LTag[Double].tag)
    
    bh.consume(stringTag)
    bh.consume(intTag)
    bh.consume(booleanTag)
    bh.consume(longTag)
    bh.consume(doubleTag)
  }
}

