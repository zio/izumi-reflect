package izumi.reflect.test

import izumi.reflect._
import scala.util.Random

object QuickPerformanceTest {
  def main(args: Array[String]): Unit = {
    println("=== izumi-reflect Performance Test ===")
    println()
    
    // Warm up
    println("Warming up...")
    (1 to 1000).foreach { _ =>
      val tag1 = Tag[String]
      val tag2 = Tag[List[String]]  
      val tag3 = Tag[Map[String, Int]]
    }
    
    // Test simple tags
    println("Testing simple tag creation...")
    val start1 = System.nanoTime()
    (1 to 10000).foreach { _ =>
      val tag1 = Tag[String]
      val tag2 = Tag[Int] 
      val tag3 = Tag[Boolean]
    }
    val end1 = System.nanoTime()
    val simpleTime = (end1 - start1) / 1000000.0
    println(f"Simple tags: $simpleTime%.2f ms (${30000 / simpleTime * 1000}%.0f ops/sec)")
    
    // Test collection tags  
    println("Testing collection tag creation...")
    val start2 = System.nanoTime()
    (1 to 10000).foreach { _ =>
      val tag1 = Tag[List[String]]
      val tag2 = Tag[Set[Int]]
      val tag3 = Tag[Map[String, Int]]
    }
    val end2 = System.nanoTime()
    val collectionTime = (end2 - start2) / 1000000.0
    println(f"Collection tags: $collectionTime%.2f ms (${30000 / collectionTime * 1000}%.0f ops/sec)")
    
    // Test complex nested tags
    println("Testing complex nested tag creation...")
    val start3 = System.nanoTime()
    (1 to 5000).foreach { _ =>
      val tag1 = Tag[Map[String, Either[Exception, List[String]]]]
      val tag2 = Tag[Either[Exception, Option[List[Int]]]]
    }
    val end3 = System.nanoTime()
    val complexTime = (end3 - start3) / 1000000.0
    println(f"Complex tags: $complexTime%.2f ms (${10000 / complexTime * 1000}%.0f ops/sec)")
    
    // Test repeated creation (should show cache benefits)
    println("Testing repeated tag creation (cache effectiveness)...")
    val start4 = System.nanoTime()
    (1 to 50000).foreach { _ =>
      val tag = Tag[String] // Same tag repeatedly
    }
    val end4 = System.nanoTime()
    val repeatedTime = (end4 - start4) / 1000000.0
    println(f"Repeated tags: $repeatedTime%.2f ms (${50000 / repeatedTime * 1000}%.0f ops/sec)")
    
    println()
    println("=== Summary ===")
    println(f"Simple tag creation: ${30000 / simpleTime * 1000}%.0f ops/sec")
    println(f"Collection tags: ${30000 / collectionTime * 1000}%.0f ops/sec") 
    println(f"Complex nested tags: ${10000 / complexTime * 1000}%.0f ops/sec")
    println(f"Repeated tags (cache hit): ${50000 / repeatedTime * 1000}%.0f ops/sec")
    
    val cacheEffectiveness = (50000 / repeatedTime) / (30000 / simpleTime)
    println(f"Cache effectiveness: ${cacheEffectiveness}x faster for repeated operations")
  }
}