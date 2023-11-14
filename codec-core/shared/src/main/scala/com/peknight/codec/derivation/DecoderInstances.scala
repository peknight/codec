package com.peknight.codec.derivation

import cats.Monad
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.priority.LowPriority

trait DecoderInstances:
  given derivedDecoder[F[_], T, E, A](using
    configuration: DecoderConfiguration,
    monad: Monad[F],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): LowPriority[Decoder[F, T, E, A]] =
    LowPriority(DecoderDerivationInstances.derived[F, T, E, A])
end DecoderInstances
object DecoderInstances extends DecoderInstances
