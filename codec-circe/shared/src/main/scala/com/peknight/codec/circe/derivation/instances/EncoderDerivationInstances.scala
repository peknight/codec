package com.peknight.codec.circe.derivation.instances

import com.peknight.codec.circe.derivation.EncoderDerivation
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.Json
import io.circe.`export`.Exported

trait EncoderDerivationInstances:
  given derivedEncoder[A](using
   configuration: EncoderConfiguration,
   instances: => Generic.Instances[[X] =>> Encoder[Json, X], A]
  ): Exported[io.circe.Encoder[A]] = Exported(EncoderDerivation.derived[A])
end EncoderDerivationInstances
object EncoderDerivationInstances extends EncoderDerivationInstances
