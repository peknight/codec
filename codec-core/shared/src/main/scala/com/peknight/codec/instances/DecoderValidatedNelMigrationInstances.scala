package com.peknight.codec.instances

import cats.Functor
import cats.data.ValidatedNel
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.MidPriority

trait DecoderValidatedNelMigrationInstances extends DecoderIdMigrationInstances:
  given validatedNelMigrationDecoder[F[_], T, E, A](using functor: Functor[F],
                                                    migration: Migration[[X] =>> F[ValidatedNel[E, X]], T, A])
  : MidPriority[Decoder[F, T, E, A]] =
    MidPriority {
      new Decoder[F, T, E, A]:
        def decode(t: T): F[Either[E, A]] = migration.migrate(t).map(_.toEither.left.map(_.head))
        def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = migration.migrate(t)
    }
  end validatedNelMigrationDecoder
end DecoderValidatedNelMigrationInstances

