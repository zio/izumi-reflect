/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 */

package izumi.reflect.internal.cache

import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import scala.language.reflectiveCalls

/** Utility for deterministic SHA-1 cache key generation. */
object CacheKeyGen {
  def hashKey(structural: String): String = {
    val digest = MessageDigest.getInstance("SHA-1")
    val hashBytes = digest.digest(structural.getBytes(StandardCharsets.UTF_8))
    bytesToHex(hashBytes)
  }

  private def bytesToHex(bytes: Array[Byte]): String = {
    val hex = new StringBuilder(bytes.length * 2)
    for (b <- bytes) hex.append("%02x".format(b & 0xff))
    hex.toString
  }

  def compositeKey(parts: String*): String =
    hashKey(parts.mkString("|||"))
}

/** Stable cache keys for Scala 2 Type. */
object Scala2KeyGen {
  def stableKey(tpe: Any): String = {
    val normalized = tpe.asInstanceOf[{ def dealias: Any; def toString: String }].dealias.toString
    CacheKeyGen.hashKey(s"scala2:$normalized")
  }

  def compositeKey(types: Any*): String = {
    val normalized = types
      .map(_.asInstanceOf[{ def dealias: Any; def toString: String }].dealias.toString)
      .sorted
      .mkString("|")
    CacheKeyGen.hashKey(s"scala2-composite:$normalized")
  }
}