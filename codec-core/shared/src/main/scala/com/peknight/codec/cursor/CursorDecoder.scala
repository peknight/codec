package com.peknight.codec.cursor

import cats.Applicative
import com.peknight.codec.cursor.Cursor.SuccessCursor
import com.peknight.codec.error.DecodingFailure

object CursorDecoder:
  def instance[F[_]: Applicative, S, A](f: SuccessCursor[S] => F[Either[DecodingFailure[Cursor[S]], A]])
  : Decoder[F, S, A] =
    Decoder.instance[F, S, A](f)
end CursorDecoder
