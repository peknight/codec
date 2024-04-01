package com.peknight.codec.id

object Decoder:
  def apply[T, E, A](using decoder: Decoder[T, E, A]): Decoder[T, E, A] = decoder
end Decoder
