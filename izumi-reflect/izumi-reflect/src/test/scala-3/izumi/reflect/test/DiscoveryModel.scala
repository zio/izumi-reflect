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

  type GetDiscoveryNode[T <: DiscoverableService] <: NodeId = T match {
    case GetDiscoveryNodePattern[t] => t
  }
  type GetDiscoveryNodePattern[Id <: NodeId] = DiscoverableService { type DiscoveryNode = Id }

}
