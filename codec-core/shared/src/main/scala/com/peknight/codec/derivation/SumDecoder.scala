package com.peknight.codec.derivation

import com.peknight.codec.Decoder

trait SumDecoder[F[_], T, E, A] extends Decoder[F, T, E, A]:
  def decoders: Map[String, Decoder[F, T, E, _]]
end SumDecoder
