package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{CirceTypeInstances, EncoderCirceInstances}
import com.peknight.codec.circe.syntax.codec.asCirceEncoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.{Json, JsonObject}

trait EncoderDerivation extends CirceTypeInstances with EncoderCirceInstances:
  def derived[A](using configuration: EncoderConfiguration)(using
    instances: => Generic.Instances[[X] =>> Encoder[Json, X], A]
  ): io.circe.Encoder[A] =
    com.peknight.codec.derivation.EncoderDerivation.derived[Id, Json, JsonObject, A].asCirceEncoder
end EncoderDerivation
object EncoderDerivation extends EncoderDerivation
