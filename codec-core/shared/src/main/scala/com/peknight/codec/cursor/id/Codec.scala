package com.peknight.codec.cursor.id

object Codec:
  def apply[S, A](using codec: Codec[S, A]): Codec[S, A] = codec
end Codec
