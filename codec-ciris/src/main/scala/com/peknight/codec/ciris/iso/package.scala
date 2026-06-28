package com.peknight.codec.ciris

import cats.syntax.applicative.*
import cats.{Applicative, Id}
import ciris.{ConfigDecoder, ConfigError}
import com.peknight.codec.Decoder
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.migration.Isomorphism

package object iso:
  given decoderIsomorphism[F[_]: Applicative, T, A]: Isomorphism[F, Decoder[Id, T, A], ConfigDecoder[T, A]] with
    def to(a: Decoder[Id, T, A]): F[ConfigDecoder[T, A]] =
      ConfigDecoder.instance[T, A] { (key, t) =>
        a.decode(t).left.map { error =>
          ConfigError(key.fold(error)(k => error.prependLabel(k.description)).message)
        }
      }.pure
    def from(b: ConfigDecoder[T, A]): F[Decoder[Id, T, A]] =
      Decoder.applicative[Id, T, A] { t =>
        b.decode(None, t).left.map(DecodingFailure.apply)
      }.pure
  end decoderIsomorphism
end iso
