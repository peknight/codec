package com.peknight.codec.instances

import cats.{Applicative, Show}
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor

trait DecoderCursorInstances:
  given cursorIdentityDecoder[F[_]: Applicative, S: Show]: Decoder[F, Cursor[S], S] = Decoder.cursorIdentity[F, S]
end DecoderCursorInstances
