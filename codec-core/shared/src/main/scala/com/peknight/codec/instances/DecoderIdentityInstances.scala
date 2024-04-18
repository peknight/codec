package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder

trait DecoderIdentityInstances:
  given identityDecoder[F[_]: Applicative, A]: Decoder[F, A, A] = Decoder.identity[F, A]
end DecoderIdentityInstances
