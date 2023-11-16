package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.DecoderInstances
import com.peknight.codec.circe.syntax.codec.asCirceDecoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.derivation.DecoderDerivation as CodecDecoderDerivation
import com.peknight.generic.Generic
import io.circe.*

trait DecoderDerivation extends CursorTypeInstances
  with ObjectTypeInstances
  with DecodingFailureMigrationInstances
  with DecoderInstances:
  def derived[A](using configuration: DecoderConfiguration)(using instances: => Generic.Instances[Decoder, A])
  : Decoder[A] =
    CodecDecoderDerivation.derived[Id, Json, JsonObject, ACursor, DecodingFailure, A](using configuration)
      .asCirceDecoder
end DecoderDerivation
object DecoderDerivation extends DecoderDerivation

