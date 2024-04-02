package com.peknight.codec.instances.generic.decoder

import cats.Functor
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait DecoderMigrationInstances:
  given migrationDecoder[F[_], T, E, A] (using functor: Functor[F], migration: Migration[F, T, A])
  : MidPriority[Decoder[F, T, E, A]] =
    MidPriority(Decoder.migrationDecoder[F, T, E, A](migration))
  end migrationDecoder
end DecoderMigrationInstances
object DecoderMigrationInstances extends DecoderMigrationInstances
