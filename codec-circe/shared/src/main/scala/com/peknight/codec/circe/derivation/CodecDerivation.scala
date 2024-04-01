package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances.{IsomorphismInstances, JsonTypeInstances}
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.cursor.id.Decoder
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.{ACursor, DecodingFailure, Json}

trait CodecDerivation extends IsomorphismInstances with JsonTypeInstances:
  def derived[A](using configuration: CodecConfiguration)(using
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[Json, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[Json, X], A]
  ): io.circe.Codec[A] =
    codecIsomorphism.to(com.peknight.codec.derivation.CodecDerivation.derived[Id, Json, A](using configuration))
end CodecDerivation
object CodecDerivation extends CodecDerivation
