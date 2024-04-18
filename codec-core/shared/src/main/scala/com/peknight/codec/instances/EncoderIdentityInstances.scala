package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder

trait EncoderIdentityInstances:
  given identityEncoder[F[_]: Applicative, A]: Encoder[F, A, A] = Encoder.identity[F, A]
end EncoderIdentityInstances

