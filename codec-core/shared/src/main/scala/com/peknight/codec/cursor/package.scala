package com.peknight.codec

import com.peknight.codec.error.DecodingFailure

package object cursor:
  type Decoder[F[_], S, A] = com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], A]
  type CursorDecoder[F[_], S, A] = com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], A]
end cursor
