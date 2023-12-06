package com.peknight.codec.circe.derivation

trait NullTypeInstances:
  given NullType = NullType
end NullTypeInstances
object NullTypeInstances extends NullTypeInstances
