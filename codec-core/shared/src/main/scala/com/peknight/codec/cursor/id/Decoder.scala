package com.peknight.codec.cursor.id

object Decoder:
  def apply[S, A](using decoder: Decoder[S, A]): Decoder[S, A] = decoder
end Decoder
