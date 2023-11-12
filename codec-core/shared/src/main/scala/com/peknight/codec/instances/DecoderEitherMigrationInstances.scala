package com.peknight.codec.instances

import cats.Functor
import cats.data.ValidatedNel
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration

trait DecoderEitherMigrationInstances:
  given eitherMigrationDecoder[F[_], T, E, A] (using migration: Migration[[X] =>> F[Either[E, X]], T, A],
                                               functor: Functor[F]): Decoder[F, T, E, A] with
    def decode(t: T): F[Either[E, A]] = migration.migrate(t)
    def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = migration.migrate(t).map(_.toValidatedNel)
  end eitherMigrationDecoder
end DecoderEitherMigrationInstances
