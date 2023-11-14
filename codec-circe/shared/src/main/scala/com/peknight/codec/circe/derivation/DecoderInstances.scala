package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances
import com.peknight.codec.circe.instances.{CodecObjectOpsInstances, DecodingFailureMigrationInstances}
import com.peknight.codec.circe.syntax.codec.asCirceDecoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.derivation.DecoderDerivationInstances as CodecDecoderInstances
import com.peknight.generic.Generic
import io.circe.`export`.Exported
import io.circe.{ACursor, Decoder, DecodingFailure}

trait DecoderInstances:
  given derivedDecoder[A](using configuration: DecoderConfiguration, instances: => Generic.Instances[Decoder, A])
  : Exported[Decoder[A]] = Exported(DecoderInstances.derived[A])
end DecoderInstances
object DecoderInstances extends DecoderInstances
  with CodecObjectOpsInstances
  with DecodingFailureMigrationInstances
  with instances.DecoderInstances:
  def derived[A](using configuration: DecoderConfiguration)(using instances: => Generic.Instances[Decoder, A])
  : Decoder[A] =
    CodecDecoderInstances.derived[Id, ACursor, DecodingFailure, A](using configuration).asCirceDecoder
end DecoderInstances

