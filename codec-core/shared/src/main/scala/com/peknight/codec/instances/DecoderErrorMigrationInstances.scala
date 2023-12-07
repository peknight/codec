package com.peknight.codec.instances

import cats.Functor
import cats.data.ValidatedNel
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.error.Error
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.priority.MidPriority

trait DecoderErrorMigrationInstances:
  given errorMigrationDecoder[F[_], T, E, A](using functor: Functor[F], decoder: Decoder[F, T, Error, A],
                                             migration: Migration[Error, E])
  : MidPriority[Decoder[F, T, E, A]] =
    MidPriority {
      new Decoder[F, T, E, A]:
        def decode(t: T): F[Either[E, A]] = decoder.decode(t).map(_.left.map(migration.migrate))
        def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
          decoder.decodeAccumulating(t).map(_.leftMap(_.map(migration.migrate)))
    }
  end errorMigrationDecoder
end DecoderErrorMigrationInstances
