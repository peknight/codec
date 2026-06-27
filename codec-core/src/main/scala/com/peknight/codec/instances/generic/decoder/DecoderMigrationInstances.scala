package com.peknight.codec.instances.generic.decoder

import cats.Functor
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait DecoderMigrationInstances:
  given decodeWithMigrationM[F[_], T, A](using functor: Functor[F], migration: Migration[F, T, A])
  : MidPriority[Decoder[F, T, A]] =
    MidPriority(Decoder.decodeWithMigration[F, T, A])
  end decodeWithMigrationM
end DecoderMigrationInstances
object DecoderMigrationInstances extends DecoderMigrationInstances
