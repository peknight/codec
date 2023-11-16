package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asEncoder
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.{Json, Encoder as CirceEncoder}

trait EncoderInstances:
  given circeEncoder[A](using encoder: CirceEncoder[A]): Encoder[Json, A] = encoder.asEncoder
  given genericEncoderInstances[A](using instances: => Generic.Instances[CirceEncoder, A])
  : Generic.Instances[[X] =>> Encoder[Json, X], A] =
    instances.mapK[[X] =>> Encoder[Json, X]]([X] => (encoder: CirceEncoder[X]) => encoder.asEncoder)
end EncoderInstances
object EncoderInstances extends EncoderInstances
