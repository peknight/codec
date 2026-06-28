package com.peknight.codec.ciris.instances

import cats.Id
import ciris.ConfigDecoder
import com.peknight.codec.Decoder
import com.peknight.codec.ciris.iso.decoderIsomorphism

trait ConfigDecoderInstances:
  given stringDecoderAsConfigDecoder[A](using decoder: Decoder[Id, String, A]): ConfigDecoder[String, A] =
    decoderIsomorphism[Id, String, A].to(decoder)
end ConfigDecoderInstances
object ConfigDecoderInstances extends ConfigDecoderInstances
