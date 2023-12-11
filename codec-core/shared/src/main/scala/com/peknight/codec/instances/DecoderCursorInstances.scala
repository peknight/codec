package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.cursor.{Cursor, Decoder}
import com.peknight.codec.error.DecodingFailure

trait DecoderCursorInstances:
  given decodeS[F[_]: Applicative, S]: Decoder[F, S, S] = Decoder.instance[F, S, S](_.value.asRight.pure)
end DecoderCursorInstances
