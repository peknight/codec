package com.peknight.codec.instances.generic.decoder

import cats.{Monad, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.config.DecoderConfig
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait DecoderDerivationInstances:
  given derivedDecoder[F[_], S, A](using
    config: DecoderConfig,
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): LowPriority[Decoder[F, Cursor[S], A]] =
    LowPriority(Decoder.derived[F, S, A])
end DecoderDerivationInstances
object DecoderDerivationInstances extends DecoderDerivationInstances
