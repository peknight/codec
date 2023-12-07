package com.peknight.codec.instances

import com.peknight.codec.Encoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait EncoderMigrationInstances:
  given migrationEncoder[F[_], S, A](using migration: Migration[F, A, S]): MidPriority[Encoder[F, S, A]] =
    MidPriority(migration.migrate(_))
  end migrationEncoder
end EncoderMigrationInstances
