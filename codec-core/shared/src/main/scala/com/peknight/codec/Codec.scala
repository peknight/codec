package com.peknight.codec

import com.peknight.codec.derivation.CodecDerivation

trait Codec[F[_], S, T, E, A] extends Encoder[F, S, A] with Decoder[F, T, E, A]
object Codec extends CodecDerivation
