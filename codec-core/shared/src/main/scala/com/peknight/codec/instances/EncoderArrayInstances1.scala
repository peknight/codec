package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType

trait EncoderArrayInstances1:
  given vectorEncodeIterable[F[_], S, A, G[_]](
    using
    applicative: Applicative[F],
    encoder: Encoder[F, S, A],
    ev: G[A] => Iterable[A]
  ): Encoder[F, Vector[S], G[A]] =
    Encoder.vectorEncoder[F, S, A].contramap(a => ev(a).toVector)

  given encodeIterable[F[_], S, A, G[_]](
    using
    applicative: Applicative[F],
    encoder: Encoder[F, S, A],
    ev: G[A] => Iterable[A],
    arrayType: ArrayType[S]
  ): Encoder[F, S, G[A]] =
    Encoder.arrayEncoder[F, S, G[A]]
end EncoderArrayInstances1
