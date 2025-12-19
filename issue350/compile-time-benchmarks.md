# Compile-Time Cache Implementation for Issue #350

> **Issue**: [Granular compile-time caches for Scala 2 and 3 #350](https://github.com/zio/izumi-reflect/issues/350)

## Benchmark Results

**Environment**: Windows, JDK 17, sbt 1.11.2  
**Target**: `izumi-reflectJVM/Test/compile`  
**Date**: December 20, 2025

### Scala 3.3.6

| Build Type | No Cache | With Cache | Improvement |
|------------|----------|------------|-------------|
| Clean | 49.07s | 47.95s | **2.3%** |

### Scala 2.13.14

| Build Type | No Cache | With Cache | Improvement |
|------------|----------|------------|-------------|
| Clean | 43.90s | 41.02s | **6.6%** |
