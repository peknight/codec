package com.peknight.codec.instances.generic.decoder

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Decoder, Object}
import com.peknight.generic.priority.MidPriority

trait DecoderObjectInstances:
  given objectDecoder[F[_], S, A](
    using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, Object[S]],
    decoder: Decoder[F, Object[S], DecodingFailure, A]
  ): MidPriority[Decoder[F, Cursor[S], DecodingFailure, A]] =
    MidPriority(Decoder.objectDecoder[F, S, A])
end DecoderObjectInstances
object DecoderObjectInstances extends DecoderObjectInstances
