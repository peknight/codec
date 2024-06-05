package com.peknight.codec.derivation

import com.peknight.codec.Decoder

trait SumDecoder[F[_], T, A] extends Decoder[F, T, A]:
  def decoders: Map[String, Decoder[F, T, ?]]
end SumDecoder
