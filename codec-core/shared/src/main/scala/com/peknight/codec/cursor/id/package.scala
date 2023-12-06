package com.peknight.codec.cursor

import cats.Id
import cats.data.ValidatedNel

package object id:
  type Decoder[S, A] = com.peknight.codec.Decoder[Id, Cursor[S], DecodingFailure[S], A]
  type Codec[S, A] = com.peknight.codec.Codec[Id, S, Cursor[S], DecodingFailure[S], A]
  type Result[S, A] = Either[DecodingFailure[S], A]
  type AccumulatingResult[S, A] = ValidatedNel[DecodingFailure[S], A]
end id
