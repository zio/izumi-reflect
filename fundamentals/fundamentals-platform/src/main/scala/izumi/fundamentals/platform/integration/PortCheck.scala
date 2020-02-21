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

package izumi.fundamentals.platform.integration

import java.net.{InetSocketAddress, Socket, URI, URL}

/**
  * This class is intended to be always present in the DI object graph and injected
  * into each resource which needs a port availability check.
  *
  * The timeout is intended to be defined just once per app.
  */
class PortCheck(timeout: Int) {
  def checkUrl(uri: URL, clue: String, defaultPort: Int): ResourceCheck = {
    checkUrl(uri, Some(clue), Some(defaultPort))
  }

  def checkUrl(uri: URL, clue: String): ResourceCheck = {
    checkUrl(uri, Some(clue), None)
  }

  def checkUri(uri: URI, defaultPort: Int, clue: String): ResourceCheck = {
    checkUri(uri, defaultPort, Some(clue))
  }

  def checkPort(host: String, port: Int, clue: String): ResourceCheck = {
    checkPort(host, port, Some(clue))
  }

  def checkUrl(uri: URL, clue: Option[String] = None, defaultPort: Option[Int] = None): ResourceCheck = {
    val portOrDefault: Int = portFor(uri.getDefaultPort, defaultPort, uri.getPort)
    checkPort(uri.getHost, portOrDefault, clue)
  }

  def checkUri(uri: URI, defaultPort: Int, clue: Option[String] = None): ResourceCheck = {
    val portOrDefault: Int = portFor(defaultPort, Some(defaultPort), uri.getPort)
    checkPort(uri.getHost, portOrDefault, clue)
  }

  def checkPort(host: String, port: Int, clue: Option[String] = None): ResourceCheck = {
    try {
      val socket = new Socket()
      try {
        socket.connect(new InetSocketAddress(host, port), timeout)
        ResourceCheck.Success()
      } finally {
        socket.close()
      }
    } catch {
      case t: Throwable =>
        val message = clue match {
          case Some(_) =>
            s"$clue: port check failed on $host:$port, timeout: $timeout ms"
          case None =>
            s"Port check failed on $host:$port, timeout: $timeout ms"
        }

        ResourceCheck.ResourceUnavailable(message, Some(t))
    }
  }

  private def portFor(uriPort: => Int, defaultPort: Option[Int], port: Int) = {
    val portOrDefault = port match {
      case -1 =>
        defaultPort.getOrElse(uriPort)
      case v =>
        v
    }
    portOrDefault
  }

}
