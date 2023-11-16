package com.peknight.codec.circe.derivation

import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.generic.Generic
import io.circe.Encoder
import io.circe.`export`.Exported

trait EncoderInstances:
  given derivedEncoder[A](using configuration: EncoderConfiguration, instances: => Generic.Instances[Encoder, A])
  : Exported[Encoder[A]] = Exported(EncoderDerivation.derived[A])
end EncoderInstances
object EncoderInstances extends EncoderInstances
