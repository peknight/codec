package com.peknight.codec.ciris.instances

import ciris.ConfigDecoder
import com.peknight.codec.ciris.iso.decoderIsomorphism
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.Decoder

trait ConfigDecoderInstances:
  given stringKeyEitherDecoder[A](using decoder: Decoder[String, DecodingFailure, A]): ConfigDecoder[String, A] =
    decoderIsomorphism.to(decoder)
end ConfigDecoderInstances
object ConfigDecoderInstances extends ConfigDecoderInstances
