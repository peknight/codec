package com.peknight.codec.instances

import cats.Monad
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.derivation.DecoderDerivation
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait DecoderDerivationInstances extends DecoderDerivation:
  given derivedDecoder[F[_], S, A](using
    configuration: DecoderConfiguration,
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): LowPriority[Decoder[F, Cursor[S], A]] =
    LowPriority(derived[F, S, A])
end DecoderDerivationInstances
object DecoderDerivationInstances extends DecoderDerivationInstances
