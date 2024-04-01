package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.iso.encoderIsomorphism
import com.peknight.codec.circe.sum.given
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.Json

trait EncoderDerivation:
  def derived[A](using configuration: EncoderConfiguration)(using
    instances: => Generic.Instances[[X] =>> Encoder[Json, X], A]
  ): io.circe.Encoder[A] =
    encoderIsomorphism.to(com.peknight.codec.derivation.EncoderDerivation.derived[Id, Json, A])
end EncoderDerivation
object EncoderDerivation extends EncoderDerivation
