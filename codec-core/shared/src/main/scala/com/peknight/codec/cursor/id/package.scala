package com.peknight.codec.cursor

import cats.Id
import com.peknight.codec.error.DecodingFailure

package object id:
  type Codec[S, A] = com.peknight.codec.Codec[Id, S, Cursor[S], DecodingFailure, A]
  type Decoder[S, A] = com.peknight.codec.Decoder[Id, Cursor[S], DecodingFailure, A]
end id
