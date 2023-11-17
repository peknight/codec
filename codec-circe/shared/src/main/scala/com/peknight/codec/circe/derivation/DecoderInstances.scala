package com.peknight.codec.circe.derivation

import com.peknight.codec.circe.instances
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.id.Decoder
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{ACursor, DecodingFailure, Decoder as CirceDecoder}

trait DecoderInstances extends instances.DecoderInstances:
  given derivedDecoder[A](using
   configuration: DecoderConfiguration,
   instances: => Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A]
  ): Exported[CirceDecoder[A]] = Exported(DecoderDerivation.derived[A])
end DecoderInstances
object DecoderInstances extends DecoderInstances

