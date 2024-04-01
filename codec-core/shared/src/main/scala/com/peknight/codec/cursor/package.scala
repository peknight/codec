package com.peknight.codec

import com.peknight.codec.error.DecodingFailure

package object cursor:
  type Codec[F[_], S, A] = com.peknight.codec.Codec[F, S, Cursor[S], DecodingFailure, A]
  type Decoder[F[_], S, A] = com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure, A]
end cursor
