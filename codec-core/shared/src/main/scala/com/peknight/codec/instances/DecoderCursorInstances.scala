package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.{AccumulatingResult, Cursor, Decoder, Result}

trait DecoderCursorInstances:
  given decodeS[F[_]: Applicative, S]: Decoder[F, S, S] with
    def decode(t: Cursor[S]): Result[F, S, S] = ???
    def decodeAccumulating(t: Cursor[S]): AccumulatingResult[F, S, S] = ???
  end decodeS

end DecoderCursorInstances
