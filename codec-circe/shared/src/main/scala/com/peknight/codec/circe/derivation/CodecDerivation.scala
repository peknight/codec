package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{DecoderInstances, EncoderInstances}
import com.peknight.codec.circe.syntax.codec.asCirceCodec
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.derivation.CodecDerivation as CodecCodecDerivation
import com.peknight.codec.id.{Decoder, Encoder}
import com.peknight.generic.Generic
import io.circe.{ACursor, DecodingFailure, Json, JsonObject, Codec as CirceCodec, Decoder as CirceDecoder, Encoder as CirceEncoder}

trait CodecDerivation extends CursorTypeInstances
  with ObjectTypeInstances
  with DecodingFailureMigrationInstances
  with EncoderInstances
  with DecoderInstances:
  def derived[A](using configuration: CodecConfiguration)(using
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[Json, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A]
  ): CirceCodec[A] =
    CodecCodecDerivation.derived[Id, Json, JsonObject, ACursor, DecodingFailure, A](using configuration).asCirceCodec
end CodecDerivation
object CodecDerivation extends CodecDerivation
