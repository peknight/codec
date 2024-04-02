package com.peknight.codec.cursor

object Codec:
  def apply[F[_], S, A](using codec: Codec[F, S, A]): Codec[F, S, A] = codec
end Codec
