package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances
import com.peknight.codec.circe.instances.ObjectTypeInstances
import com.peknight.codec.circe.syntax.codec.asCirceEncoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.derivation.EncoderDerivationInstances as CodecEncoderInstances
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{Encoder, Json, JsonObject}

trait EncoderInstances:
  given derivedEncoder[A](using configuration: EncoderConfiguration, instances: => Generic.Instances[Encoder, A])
  : Exported[Encoder[A]] = Exported(EncoderInstances.derived[A])
end EncoderInstances
object EncoderInstances extends EncoderInstances with ObjectTypeInstances with instances.EncoderInstances:
  def derived[A](using configuration: EncoderConfiguration)(using instances: => Generic.Instances[Encoder, A])
  : Encoder[A] =
    CodecEncoderInstances.derived[Id, Json, JsonObject, A](using configuration).asCirceEncoder
end EncoderInstances
