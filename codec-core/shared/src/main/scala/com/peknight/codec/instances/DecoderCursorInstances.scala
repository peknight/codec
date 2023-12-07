package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.cursor.{Cursor, Decoder, SuccessCursor}
import com.peknight.codec.error.DecodingFailure

trait DecoderCursorInstances:
  given decodeS[F[_]: Applicative, S]: Decoder[F, S, S] with
    def apply(t: SuccessCursor[S]): F[Either[DecodingFailure[Cursor[S]], S]] = t.value.asRight.pure
  end decodeS
end DecoderCursorInstances
