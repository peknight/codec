package com.peknight.codec.derivation

import cats.Functor
import cats.data.ValidatedNel
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons
import com.peknight.generic.migration.id.Migration

trait EnumCodecDerivation:
  inline def derived[F[_], S, T, E, A](using configuration: CodecConfiguration)(using
    functor: Functor[F],
    failure: Migration[DecodingFailure, E],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, E, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, E, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new Codec[F, S, T, E, A] with EnumDecoder[F, T, E, A]:
      def decoders: Map[String, Decoder[F, T, E, _]] =
        EnumDecoderDerivation.enumDecodersDict[F, T, E, A](this, configuration, generic)
      def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
      def decode(t: T): F[Either[E, A]] =
        EnumDecoderDerivation.decodeEnumEither[F, T, E, A, generic.Repr](t, configuration, failure, stringDecoder,
          generic, singletons)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        EnumDecoderDerivation.decodeEnumValidatedNel[F, T, E, A, generic.Repr](t, configuration, failure, stringDecoder,
          generic, singletons)
  end derived
end EnumCodecDerivation
object EnumCodecDerivation extends EnumCodecDerivation
