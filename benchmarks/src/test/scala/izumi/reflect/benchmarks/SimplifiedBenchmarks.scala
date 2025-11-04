/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 */

package izumi.reflect.benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import izumi.reflect.*

/**
 * Comprehensive benchmarks for izumi-reflect Tag creation performance.
 * 
 * These benchmarks measure the performance of creating Tag instances
 * for various type scenarios, which benefits from the caching implementation.
 */
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Fork(1)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class SimplifiedBenchmarks {

  @Benchmark
  def simpleTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[String]
    val tag2 = Tag[Int]
    val tag3 = Tag[Boolean]
    val tag4 = Tag[Long]
    val tag5 = Tag[Double]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
    bh.consume(tag4)
    bh.consume(tag5)
  }

  @Benchmark
  def collectionTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[List[String]]
    val tag2 = Tag[Set[Int]]
    val tag3 = Tag[Map[String, Int]]
    val tag4 = Tag[Vector[Boolean]]
    val tag5 = Tag[Array[String]]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
    bh.consume(tag4)
    bh.consume(tag5)
  }

  @Benchmark
  def nestedGenericTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[Option[String]]
    val tag2 = Tag[Either[String, Int]]
    val tag3 = Tag[Try[String]]
    val tag4 = Tag[Future[String]]
    val tag5 = Tag[Option[List[String]]]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
    bh.consume(tag4)
    bh.consume(tag5)
  }

  @Benchmark
  def complexNestedTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[Map[String, Either[Exception, List[String]]]]
    val tag2 = Tag[Either[Exception, Option[List[Int]]]]
    val tag3 = Tag[Future[Either[String, Map[String, List[Int]]]]]
    val tag4 = Tag[Option[Either[Exception, Map[String, Set[Int]]]]]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
    bh.consume(tag4)
  }

  @Benchmark
  def functionTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[String => Int]
    val tag2 = Tag[(String, Int) => Boolean]
    val tag3 = Tag[Int => Option[String]]
    val tag4 = Tag[List[String] => Set[Int]]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
    bh.consume(tag4)
  }

  @Benchmark
  def repeatedTagCreation(bh: Blackhole): Unit = {
    // This should benefit significantly from caching
    for (i <- 1 to 20) {
      val tag1 = Tag[String]
      val tag2 = Tag[List[String]]
      val tag3 = Tag[Map[String, Int]]
      val tag4 = Tag[Either[Exception, String]]
      
      bh.consume(tag1)
      bh.consume(tag2)
      bh.consume(tag3)
      bh.consume(tag4)
    }
  }

  @Benchmark 
  def tagComparison(bh: Blackhole): Unit = {
    val tag1 = Tag[String]
    val tag2 = Tag[String]
    val tag3 = Tag[Int]
    
    val result1 = tag1 == tag2
    val result2 = tag1 == tag3
    val result3 = tag1.hashCode() == tag2.hashCode()
    
    bh.consume(result1)
    bh.consume(result2)
    bh.consume(result3)
  }

  @Benchmark
  def tagInspection(bh: Blackhole): Unit = {
    val tag = Tag[Map[String, Either[Exception, List[Int]]]]
    
    val closestClass = tag.closestClass
    val hashCode = tag.hashCode()
    val toString = tag.toString
    
    bh.consume(closestClass)
    bh.consume(hashCode)
    bh.consume(toString)
  }

  // Test concurrent access to tag creation
  @Benchmark
  @Threads(4)
  def concurrentTagCreation(bh: Blackhole): Unit = {
    val tag1 = Tag[String]
    val tag2 = Tag[List[String]]
    val tag3 = Tag[Map[String, Int]]
    
    bh.consume(tag1)
    bh.consume(tag2)
    bh.consume(tag3)
  }

  // Test scenarios that simulate framework usage patterns
  @Benchmark
  def frameworkPatternTags(bh: Blackhole): Unit = {
    // DI container patterns
    trait Repository[T]
    trait Service[T]
    trait Controller[T]
    
    val repoTag = Tag[Repository[String]]
    val serviceTag = Tag[Service[String]]
    val controllerTag = Tag[Controller[String]]
    
    bh.consume(repoTag)
    bh.consume(serviceTag)
    bh.consume(controllerTag)
  }
}