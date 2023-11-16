package com.peknight.codec.circe.derivation

import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.generic.Generic
import io.circe.Decoder
import io.circe.`export`.Exported

trait DecoderInstances:
  given derivedDecoder[A](using configuration: DecoderConfiguration, instances: => Generic.Instances[Decoder, A])
  : Exported[Decoder[A]] = Exported(DecoderDerivation.derived[A])
end DecoderInstances
object DecoderInstances extends DecoderInstances

