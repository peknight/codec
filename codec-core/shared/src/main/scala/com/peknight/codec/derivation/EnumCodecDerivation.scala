package com.peknight.codec.derivation

import cats.{Applicative, Functor}
import com.peknight.codec.configuration.Configuration
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumCodecDerivation:
  inline def derived[F[_], S, T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new Codec[F, S, T, A] with EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, ?]] =
        EnumDecoderDerivation.enumDecodersDict[F, T, A](this, configuration, generic)
      def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        EnumDecoderDerivation.decodeEnum[F, T, A](t, configuration, stringDecoder, generic)(singletons)
  end derived
  def unsafeDerived[F[_], S, T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, A] =
    new Codec[F, S, T, A] with EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, ?]] =
        EnumDecoderDerivation.enumDecodersDict[F, T, A](this, configuration, generic)
      def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        EnumDecoderDerivation.unsafeDecodeEnum[F, T, A, generic.Repr](t, configuration, stringDecoder, generic)
  end unsafeDerived

  inline def derivedStringCodecEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Codec[F, String, String, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new Codec[F, String, String, A] with EnumDecoder[F, String, A]:
      def decoders: Map[String, Decoder[F, String, ?]] =
        EnumDecoderDerivation.enumDecodersDict[F, String, A](this, configuration, generic)
      def encode(a: A): F[String] = EnumEncoderDerivation.stringEncodeEnum(a, configuration, applicative, generic)
      def decode(t: String): F[Either[DecodingFailure, A]] =
        EnumDecoderDerivation.stringDecodeEnum[F, A](t, configuration, generic)(singletons)
  end derivedStringCodecEnum

  def unsafeDerivedStringCodecEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Codec[F, String, String, A] =
    new Codec[F, String, String, A] with EnumDecoder[F, String, A]:
      def decoders: Map[String, Decoder[F, String, ?]] =
        EnumDecoderDerivation.enumDecodersDict[F, String, A](this, configuration, generic)
      def encode(a: A): F[String] = EnumEncoderDerivation.stringEncodeEnum(a, configuration, applicative, generic)
      def decode(t: String): F[Either[DecodingFailure, A]] =
        EnumDecoderDerivation.unsafeStringDecodeEnum(t, configuration, generic)
  end unsafeDerivedStringCodecEnum
end EnumCodecDerivation
object EnumCodecDerivation extends EnumCodecDerivation
