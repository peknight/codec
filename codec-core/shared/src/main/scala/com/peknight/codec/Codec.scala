package com.peknight.codec

trait Codec[F[_], S, T, E, A] extends Encoder[F, S, A] with Decoder[F, T, E, A]
