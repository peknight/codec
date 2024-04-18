package com.peknight.codec.instances.generic.encoder

import com.peknight.codec.Encoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait EncoderMigrationInstances:
  given encodeWithMigrationM[F[_], S, A](using migration: Migration[F, A, S]): MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.encodeWithMigration[F, S, A])
  end encodeWithMigrationM
end EncoderMigrationInstances
object EncoderMigrationInstances extends EncoderMigrationInstances
