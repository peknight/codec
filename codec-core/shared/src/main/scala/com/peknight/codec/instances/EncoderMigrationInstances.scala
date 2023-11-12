package com.peknight.codec.instances

import com.peknight.codec.Encoder
import com.peknight.generic.migration.Migration

trait EncoderMigrationInstances:
  given migrationEncoder[F[_], S, A](using migration: Migration[F, A, S]): Encoder[F, S, A] with
    def encode(a: A): F[S] = migration.migrate(a)
  end migrationEncoder
end EncoderMigrationInstances
