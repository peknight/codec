package com.peknight.codec.circe.instances

import com.peknight.codec.circe.derivation.DecoderDerivation
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.id.Decoder
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{ACursor, DecodingFailure}

trait DecoderDerivationInstances extends DecoderCirceInstances:
  given derivedDecoder[A](using
   configuration: DecoderConfiguration,
   instances: => Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A]
  ): Exported[io.circe.Decoder[A]] = Exported(DecoderDerivation.derived[A])
end DecoderDerivationInstances
object DecoderDerivationInstances extends DecoderDerivationInstances

