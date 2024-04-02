package com.peknight.codec.id

object Codec:
  def apply[S, T, E, A](using codec: Codec[S, T, E, A]): Codec[S, T, E, A] = codec
end Codec
