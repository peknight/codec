package com.peknight.codec.id

object Encoder:
  def apply[S, A](using encoder: Encoder[S, A]): Encoder[S, A] = encoder
end Encoder
