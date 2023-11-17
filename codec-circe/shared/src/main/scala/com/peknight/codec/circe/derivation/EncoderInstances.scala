package com.peknight.codec.circe.derivation

import com.peknight.codec.circe.instances
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.id.Encoder
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{Json, Encoder as CirceEncoder}

trait EncoderInstances extends instances.EncoderInstances:
  given derivedEncoder[A](using
   configuration: EncoderConfiguration,
   instances: => Generic.Instances[[X] =>> Encoder[Json, X], A]
  ): Exported[CirceEncoder[A]] = Exported(EncoderDerivation.derived[A])
end EncoderInstances
object EncoderInstances extends EncoderInstances
