package com.peknight.codec.instances

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.validated.*
import com.peknight.codec.Decoder

trait DecoderIdentityInstances:
  given decodeIdentity[F[_]: Applicative, E, A]: Decoder[F, A, E, A] with
    override def decode(t: A): F[Either[E, A]] = t.asRight[E].pure[F]
    override def decodeAccumulating(t: A): F[ValidatedNel[E, A]] = t.validNel[E].pure[F]
  end decodeIdentity
end DecoderIdentityInstances
