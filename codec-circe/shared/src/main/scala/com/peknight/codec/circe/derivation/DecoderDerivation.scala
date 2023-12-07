package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{CirceTypeInstances, DecoderCirceInstances, DecodingFailureMigrationInstances}
import com.peknight.codec.circe.syntax.codec.asCirceDecoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.id.{Decoder, Encoder}
import com.peknight.generic.Generic
import io.circe.{ACursor, DecodingFailure, Json, JsonObject}

trait DecoderDerivation extends CirceTypeInstances
  with DecodingFailureMigrationInstances
  with DecoderCirceInstances:
  def derived[A](using configuration: DecoderConfiguration)(using
    instances: => Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A]
  ): io.circe.Decoder[A] =
    com.peknight.codec.derivation.DecoderDerivation
      .derived[Id, Json, JsonObject, ACursor, DecodingFailure, A].asCirceDecoder
end DecoderDerivation
object DecoderDerivation extends DecoderDerivation

