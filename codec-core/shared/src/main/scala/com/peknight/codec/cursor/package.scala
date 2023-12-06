package com.peknight.codec

import cats.data.ValidatedNel

package object cursor:
  type DecodingFailure[S] = com.peknight.codec.error.DecodingFailure[Cursor[S]]
  type Decoder[F[_], S, A] = com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[S], A]
  type Codec[F[_], S, A] = com.peknight.codec.Codec[F, S, Cursor[S], DecodingFailure[S], A]
  type Result[F[_], S, A] = F[Either[DecodingFailure[S], A]]
  type AccumulatingResult[F[_], S, A] = F[ValidatedNel[DecodingFailure[S], A]]
end cursor
