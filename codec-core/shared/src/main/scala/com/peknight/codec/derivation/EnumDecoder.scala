package com.peknight.codec.derivation

import com.peknight.codec.Decoder
import com.peknight.generic.Generic

trait EnumDecoder[F[_], T, E, A] extends Decoder[F, T, E, A]:
  def generic: Generic.Sum[A]
end EnumDecoder
