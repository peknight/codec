package com.peknight.codec.instances

import cats.Functor
import cats.data.{Validated, ValidatedNel}
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait DecoderValidatedMigrationInstances extends DecoderValidatedNelMigrationInstances:
  given validatedMigrationDecoder[F[_], T, E, A] (using functor: Functor[F],
                                                  migration: Migration[[X] =>> F[Validated[E, X]], T, A])
  : MidPriority[Decoder[F, T, E, A]] =
    MidPriority {
      new Decoder[F, T, E, A]:
        def decode(t: T): F[Either[E, A]] = migration.migrate(t).map(_.toEither)
        def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = migration.migrate(t).map(_.toValidatedNel)
    }
  end validatedMigrationDecoder
end DecoderValidatedMigrationInstances

