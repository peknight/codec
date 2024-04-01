package com.peknight.codec.circe.instances

import com.peknight.codec.circe.sum.JsonType

trait JsonTypeInstances:
  given JsonType = JsonType
end JsonTypeInstances
object JsonTypeInstances extends JsonTypeInstances
