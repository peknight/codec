package com.peknight.codec.circe.instances

import com.peknight.codec.circe.iso.encoder
import com.peknight.codec.circe.sum.JsonTypeInstances
import com.peknight.codec.id.Encoder
import io.circe.Json
import io.circe.`export`.Exported

trait EncoderIsomorphismInstances extends JsonTypeInstances:
  given encoderIsomorphism[A](using Encoder[Json, A]): Exported[io.circe.Encoder[A]] = Exported(encoder)
end EncoderIsomorphismInstances
object EncoderIsomorphismInstances extends EncoderIsomorphismInstances