package com.peknight.codec.ip4s

package object instances:
  object all extends CidrInstances with HostInstances with PortInstances
  object cidr extends CidrInstances
  object host extends HostInstances
  object port extends PortInstances
end instances
