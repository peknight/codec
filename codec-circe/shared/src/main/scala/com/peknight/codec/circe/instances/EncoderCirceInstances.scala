package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asEncoder
import com.peknight.codec.id.Encoder
import com.peknight.generic.priority.LowPriority
import io.circe.Json

trait EncoderCirceInstances:
  given circeEncoder[A](using encoder: io.circe.Encoder[A]): LowPriority[Encoder[Json, A]] =
    LowPriority(encoder.asEncoder)
end EncoderCirceInstances
object EncoderCirceInstances extends EncoderCirceInstances
