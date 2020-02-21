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

package izumi.fundamentals.collections

import izumi.fundamentals.graphs.{AbstractGCTracer, Loops}
import org.scalatest.wordspec.AnyWordSpec

class AbstractDIGcTest extends AnyWordSpec {
  import AbstractDIGcTest._

  private val graph = Vector(
    Node(NodeId("root:1"), Set(NodeId("2"))),
    Node(NodeId("2"), Set(NodeId("3"), NodeId("4"))),
    Node(NodeId("3"), Set(NodeId("2"))),
    Node(NodeId("4"), Set(NodeId("4"))),
    Node(NodeId("5"), Set(NodeId("5")))
  )


  "abstract GC" should {
    "not loop forever on circular dependencies" in {
      val gc = new TestGCTracer(false)

      val result = gc.gc(graph)

      assert(result.reachable == Set(NodeId("root:1"), NodeId("2"), NodeId("3"), NodeId("4")))
    }

    "ignore missing dependencies when expected to" in {
      import AbstractDIGcTest._
      val gc1 = new TestGCTracer(true)

      val graph = Vector(
        Node(NodeId("root:1"), Set(NodeId("2")))
      )
      val result = gc1.gc(graph)

      assert(result.reachable == Set(NodeId("root:1"), NodeId("2")))

      val gc2 = new TestGCTracer(false)
      intercept[java.util.NoSuchElementException] {
        gc2.gc(graph)
      }
    }
  }

  "loop detector" should {
    "detect loops" in {
      val map = graph.map(n => n.id -> n.deps).toMap
      val loops = Loops.findCyclesFor(NodeId("root:1"), (n: NodeId) => map.get(n))
      assert(loops.toSet == Set(List(NodeId("root:1"), NodeId("2"), NodeId("4"), NodeId("4")), List(NodeId("root:1"), NodeId("2"), NodeId("3"), NodeId("2"))))
    }
  }
}

object AbstractDIGcTest {

  case class NodeId(id: String)

  case class Node(id: NodeId, deps: Set[NodeId])


  class TestGCTracer(override val ignoreMissingDeps: Boolean) extends AbstractGCTracer[NodeId, Node]{


    override protected def prePrune(pruned: Pruned): Pruned = {
      pruned
    }

    override protected def extractDependencies(index: Map[NodeId, Node], node: Node): Set[NodeId] = {
      node.deps
    }

    override protected def isRoot(node: NodeId): Boolean = node.id.startsWith("root:")

    override protected def id(node: Node): NodeId = node.id
  }
}
