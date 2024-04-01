package com.peknight.codec.cursor

object Decoder:
  def apply[F[_], S, A](using decoder: Decoder[F, S, A]): Decoder[F, S, A] = decoder
end Decoder
