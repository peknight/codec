package com.peknight.codec.circe.derivation

import cats.Id
import com.peknight.codec.circe.instances
import com.peknight.codec.circe.instances.{CodecObjectInstances, DecodingFailureMigrationInstances}
import com.peknight.codec.circe.syntax.codec.asCirceCodec
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.derivation.CodecInstances as CodecCodecInstances
import com.peknight.generic.Generic
import io.circe.*

object CodecInstances extends CodecObjectInstances
  with DecodingFailureMigrationInstances
  with instances.EncoderInstances
  with instances.DecoderInstances:
  def derived[A](using configuration: CodecConfiguration)(using generic: Generic[A],
                                                          encoders: => Generic.Instances[Encoder, A],
                                                          decoders: => Generic.Instances[Decoder, A]): Codec[A] =
    CodecCodecInstances.derived[Id, Json, ACursor, DecodingFailure, A](using configuration).asCirceCodec
end CodecInstances
