package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{CirceTypeInstances, DecoderCirceInstances, DecodingFailureMigrationInstances, EncoderCirceInstances}
import com.peknight.codec.circe.syntax.codec.asCirceCodec
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.id.{Decoder, Encoder}
import com.peknight.generic.Generic
import io.circe.{ACursor, DecodingFailure, Json, JsonObject}

trait CodecDerivation extends CirceTypeInstances
  with DecodingFailureMigrationInstances
  with EncoderCirceInstances
  with DecoderCirceInstances:
  def derived[A](using configuration: CodecConfiguration)(using
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[Json, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A]
  ): io.circe.Codec[A] =
    com.peknight.codec.derivation.CodecDerivation
      .derived[Id, Json, JsonObject, ACursor, DecodingFailure, A](using configuration).asCirceCodec
end CodecDerivation
object CodecDerivation extends CodecDerivation
