package com.peknight.codec.instances

import cats.Monad
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.derivation.DecoderDerivation
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.priority.LowPriority

trait DecoderDerivationInstances extends DecoderDerivation:
  given derivedDecoder[F[_], S, A](using
    configuration: DecoderConfiguration,
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], DecodingFailure, String],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): LowPriority[Decoder[F, Cursor[S], DecodingFailure, A]] =
    LowPriority(derived[F, S, A])
end DecoderDerivationInstances
object DecoderDerivationInstances extends DecoderDerivationInstances
