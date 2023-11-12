package com.peknight.codec.instances

import cats.Functor
import cats.data.ValidatedNel
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import com.peknight.codec.Decoder
import com.peknight.generic.migration.Migration

trait DecoderMigrationInstances:
  given migrationDecoder[F[_], T, E, A] (using migration: Migration[F, T, A], functor: Functor[F])
  : Decoder[F, T, E, A] with
    def decode(t: T): F[Either[E, A]] = migration.migrate(t).map(_.asRight[E])
    def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = migration.migrate(t).map(_.validNel[E])
  end migrationDecoder
end DecoderMigrationInstances
