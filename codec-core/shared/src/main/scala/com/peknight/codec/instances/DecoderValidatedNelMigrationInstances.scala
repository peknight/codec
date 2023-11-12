package com.peknight.codec.instances

import cats.Functor
import cats.data.ValidatedNel
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration

trait DecoderValidatedNelMigrationInstances:
  given validatedNelMigrationDecoder[F[_], T, E, A](using migration: Migration[[X] =>> F[ValidatedNel[E, X]], T, A],
                                                    functor: Functor[F]): Decoder[F, T, E, A] with
    def decode(t: T): F[Either[E, A]] = migration.migrate(t).map(_.toEither.left.map(_.head))
    def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = migration.migrate(t)
  end validatedNelMigrationDecoder
end DecoderValidatedNelMigrationInstances

