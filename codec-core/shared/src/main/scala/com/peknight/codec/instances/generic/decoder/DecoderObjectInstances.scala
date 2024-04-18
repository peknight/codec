package com.peknight.codec.instances.generic.decoder

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.ObjectType
import com.peknight.generic.priority.MidPriority

trait DecoderObjectInstances:
  given decodeOM[F[_], S, A](
    using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, Object[S]],
    decoder: Decoder[F, Object[S], A]
  ): MidPriority[Decoder[F, Cursor[S], A]] =
    MidPriority(Decoder.decodeO[F, S, A])
end DecoderObjectInstances
object DecoderObjectInstances extends DecoderObjectInstances
