package com.peknight.codec.derivation

import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.generic.Generic

trait SumDecoder[F[_], T, E, A] extends Decoder[F, T, E, A]:
  def configuration: DecoderConfiguration
  def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
end SumDecoder
