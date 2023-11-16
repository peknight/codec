package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{DecoderInstances, EncoderInstances}
import com.peknight.codec.circe.syntax.codec.asCirceCodec
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.derivation.CodecDerivation as CodecCodecDerivation
import com.peknight.generic.Generic
import io.circe.*

trait CodecDerivation extends CursorTypeInstances
  with ObjectTypeInstances
  with DecodingFailureMigrationInstances
  with EncoderInstances
  with DecoderInstances:
  def derived[A](using configuration: CodecConfiguration)(using
    generic: Generic[A],
    encoders: => Generic.Instances[Encoder, A],
    decoders: => Generic.Instances[Decoder, A]
  ): Codec[A] =
    CodecCodecDerivation.derived[Id, Json, JsonObject, ACursor, DecodingFailure, A](using configuration).asCirceCodec
end CodecDerivation
object CodecDerivation extends CodecDerivation
