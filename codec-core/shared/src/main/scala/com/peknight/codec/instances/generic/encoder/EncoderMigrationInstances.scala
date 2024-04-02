package com.peknight.codec.instances.generic.encoder

import com.peknight.codec.Encoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait EncoderMigrationInstances:
  given migrationEncoder[F[_], S, A](using migration: Migration[F, A, S]): MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.migrationEncoder[F, S, A])
  end migrationEncoder
end EncoderMigrationInstances
object EncoderMigrationInstances extends EncoderMigrationInstances
