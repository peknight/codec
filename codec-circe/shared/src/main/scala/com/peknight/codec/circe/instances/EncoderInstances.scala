package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asEncoder
import com.peknight.codec.id.Encoder
import com.peknight.generic.priority.LowPriority
import io.circe.{Json, Encoder as CirceEncoder}

trait EncoderInstances:
  given circeEncoder[A](using encoder: CirceEncoder[A]): LowPriority[Encoder[Json, A]] = LowPriority(encoder.asEncoder)
end EncoderInstances
object EncoderInstances extends EncoderInstances
