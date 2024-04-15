package com.peknight.codec.ciris

import cats.Id
import ciris.{ConfigDecoder, ConfigError}
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.id.Decoder
import com.peknight.generic.migration.id.Isomorphism

package object iso:
  given decoderIsomorphism[T, A]: Isomorphism[Decoder[T, DecodingFailure, A], ConfigDecoder[T, A]] with
    def to(a: Decoder[T, DecodingFailure, A]): Id[ConfigDecoder[T, A]] =
      ConfigDecoder.instance[T, A] { (key, t) =>
        a.decode(t).left.map { error =>
          ConfigError(key.fold(error)(k => error.prependLabel(k.description)).message)
        }
      }
    def from(b: ConfigDecoder[T, A]): Id[Decoder[T, DecodingFailure, A]] =
      com.peknight.codec.Decoder.instance[Id, T, DecodingFailure, A] { t =>
        b.decode(None, t).left.map(DecodingFailure.apply)
      }
  end decoderIsomorphism
end iso
