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
  inline def derived[F[_], S, T, A](using configuration: CodecConfiguration)(using
    functor: Functor[F],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, DecodingFailure, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, DecodingFailure, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new Codec[F, S, T, DecodingFailure, A] with EnumDecoder[F, T, DecodingFailure, A]:
      def decoders: Map[String, Decoder[F, T, DecodingFailure, _]] =
        EnumDecoderDerivation.enumDecodersDict[F, T, DecodingFailure, A](this, configuration, generic)
      def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        EnumDecoderDerivation.decodeEnumEither[F, T, A, generic.Repr](t, configuration, stringDecoder, generic,
          singletons)
      def decodeAccumulating(t: T): F[ValidatedNel[DecodingFailure, A]] =
        EnumDecoderDerivation.decodeEnumValidatedNel[F, T, A, generic.Repr](t, configuration, stringDecoder, generic,
          singletons)
  end derived
end EnumCodecDerivation
object EnumCodecDerivation extends EnumCodecDerivation
