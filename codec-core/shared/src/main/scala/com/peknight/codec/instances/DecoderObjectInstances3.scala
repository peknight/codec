package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Decoder, Object}

trait DecoderObjectInstances3:
  given objectDecoder[F[_], S, A](
    using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, Object[S]],
    decoder: Decoder[F, Object[S], DecodingFailure, A]
  ): Decoder[F, Cursor[S], DecodingFailure, A] =
    Decoder.objectDecoder[F, S, A](decoder)
end DecoderObjectInstances3
