package com.peknight.codec.circe.instances

import com.peknight.codec.circe.cursor.CursorType
import com.peknight.codec.circe.sum.JsonType

trait CirceTypeInstances:
  given JsonType = JsonType
  given CursorType = CursorType
end CirceTypeInstances
object CirceTypeInstances extends CirceTypeInstances
