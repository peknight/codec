package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder

trait EncoderIdentityInstances:
  given encodeIdentity[F[_]: Applicative, A]: Encoder[F, A, A] with
    def encode(a: A): F[A] = a.pure[F]
  end encodeIdentity
end EncoderIdentityInstances

