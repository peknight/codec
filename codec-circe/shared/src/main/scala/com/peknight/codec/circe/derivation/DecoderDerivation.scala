package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.iso.decoderIsomorphism
import com.peknight.codec.circe.sum.given
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.cursor.id.Decoder
import com.peknight.generic.Generic
import io.circe.Json

trait DecoderDerivation:
  def derived[A](using configuration: DecoderConfiguration)(using
    instances: => Generic.Instances[[X] =>> Decoder[Json, X], A]
  ): io.circe.Decoder[A] =
    decoderIsomorphism.to(com.peknight.codec.derivation.DecoderDerivation.derived[Id, Json, A])
end DecoderDerivation
object DecoderDerivation extends DecoderDerivation

