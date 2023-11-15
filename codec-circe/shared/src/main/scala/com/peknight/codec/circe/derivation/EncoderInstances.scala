package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances
import com.peknight.codec.circe.instances.EncodeObjectOpsInstances
import com.peknight.codec.circe.syntax.codec.asCirceEncoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.derivation.EncoderDerivationInstances as CodecEncoderInstances
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{Encoder, Json}

trait EncoderInstances:
  given derivedEncoder[A](using configuration: EncoderConfiguration, instances: => Generic.Instances[Encoder, A])
  : Exported[Encoder[A]] = Exported(EncoderInstances.derived[A])
end EncoderInstances
object EncoderInstances extends EncoderInstances with EncodeObjectOpsInstances with instances.EncoderInstances:
  def derived[A](using configuration: EncoderConfiguration)(using instances: => Generic.Instances[Encoder, A])
  : Encoder[A] =
    CodecEncoderInstances.derived[Id, Json, A](using configuration).asCirceEncoder
end EncoderInstances
