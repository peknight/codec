package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.EncoderInstances
import com.peknight.codec.circe.syntax.codec.asCirceEncoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.derivation.EncoderDerivation as CodecEncoderDerivation
import com.peknight.generic.Generic
import io.circe.{Encoder, Json, JsonObject}

trait EncoderDerivation extends ObjectTypeInstances with EncoderInstances:
  def derived[A](using configuration: EncoderConfiguration)(using instances: => Generic.Instances[Encoder, A])
  : Encoder[A] =
    CodecEncoderDerivation.derived[Id, Json, JsonObject, A](using configuration).asCirceEncoder
end EncoderDerivation
object EncoderDerivation extends EncoderDerivation
