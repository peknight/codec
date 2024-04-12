package com.peknight.codec.ciris

import cats.Id
import ciris.{ConfigDecoder, ConfigError}
import com.peknight.codec.id.Decoder
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.migration.id.Isomorphism

package object iso:
  given decodingFailureIsomorphism: Isomorphism[DecodingFailure, ConfigError] with
    def to(a: DecodingFailure): Id[ConfigError] = ConfigError(a.message)
    def from(b: ConfigError): Id[DecodingFailure] = DecodingFailure(b)
  end decodingFailureIsomorphism


  given decoderIsomorphism[T, A]: Isomorphism[Decoder[T, DecodingFailure, A], ConfigDecoder[T, A]] with
    def to(a: Decoder[T, DecodingFailure, A]): Id[ConfigDecoder[T, A]] = ???
    def from(b: ConfigDecoder[T, A]): Id[Decoder[T, DecodingFailure, A]] = ???
  end decoderIsomorphism
end iso
