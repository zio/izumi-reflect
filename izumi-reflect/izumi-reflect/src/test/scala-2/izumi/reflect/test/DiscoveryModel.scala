package izumi.reflect.test

object DiscoveryModel {

  trait NodeId
  trait DiscoverableService {
    type DiscoveryNode <: NodeId
  }
  trait DiscoveryNodeProvider[Id <: NodeId]
  trait DiscoverableServiceImpl extends DiscoverableService {
    override type DiscoveryNode = NodeIdImpl
  }
  class NodeIdImpl extends NodeId

  type GetDiscoveryNode[T <: DiscoverableService] = T#DiscoveryNode

}
