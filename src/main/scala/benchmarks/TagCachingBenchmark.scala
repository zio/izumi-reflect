package benchmarks

import izumi.reflect.TagGenerator

object TagCachingBenchmark extends App {
  // ...existing code...
  val iterations = 100000

  // Warm up
  TagGenerator.cachedTag[Int]
  TagGenerator.nonCachedTag[Int]

  // Benchmark non-cached variant
  val startNonCached = System.nanoTime()
  var sumNonCached = 0
  (1 to iterations).foreach { _ =>
    val tag = TagGenerator.nonCachedTag[Int]
    sumNonCached += tag.repr.length
  }
  val timeNonCached = System.nanoTime() - startNonCached

  // Benchmark cached variant
  val startCached = System.nanoTime()
  var sumCached = 0
  (1 to iterations).foreach { _ =>
    val tag = TagGenerator.cachedTag[Int]
    sumCached += tag.repr.length
  }
  val timeCached = System.nanoTime() - startCached

  println(s"Non-cached time: ${timeNonCached} ns")
  println(s"Cached time:   ${timeCached} ns")

}
