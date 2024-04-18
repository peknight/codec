package com.peknight.codec.circe.instances

import cats.Id
import com.peknight.codec.Encoder
import com.peknight.codec.circe.iso.encoder
import com.peknight.codec.circe.sum.JsonTypeInstances
import io.circe.Json
import io.circe.`export`.Exported

trait EncoderIsomorphismInstances extends JsonTypeInstances:
  given encoderIsomorphism[A](using Encoder[Id, Json, A]): Exported[io.circe.Encoder[A]] = Exported(encoder)
end EncoderIsomorphismInstances
object EncoderIsomorphismInstances extends EncoderIsomorphismInstances