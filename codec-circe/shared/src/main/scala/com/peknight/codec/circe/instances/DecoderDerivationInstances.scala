package com.peknight.codec.circe.instances

import com.peknight.codec.circe.derivation.DecoderDerivation
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.cursor.id.Decoder
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{ACursor, DecodingFailure, Json}

trait DecoderDerivationInstances:
  given derivedDecoder[A](using
   configuration: DecoderConfiguration,
   instances: => Generic.Instances[[X] =>> Decoder[Json, X], A]
  ): Exported[io.circe.Decoder[A]] = Exported(DecoderDerivation.derived[A])
end DecoderDerivationInstances
object DecoderDerivationInstances extends DecoderDerivationInstances

