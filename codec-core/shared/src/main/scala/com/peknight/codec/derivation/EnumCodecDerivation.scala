package com.peknight.codec.derivation

import cats.{Applicative, Functor}
import com.peknight.codec.configuration.Configuration
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumCodecDerivation:
  def enumCodecInstance[F[_], S, T, A](encode0: A => F[S])(decode0: T => F[Either[DecodingFailure, A]])
                                      (decoders0: Decoder[F, T, A] => Map[String, Decoder[F, T, ?]])
  : Codec[F, S, T, A] & EnumDecoder[F, T, A] =
    new Codec[F, S, T, A] with EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, ?]] = decoders0(this)
      def encode(a: A): F[S] = encode0(a)
      def decode(t: T): F[Either[DecodingFailure, A]] = decode0(t)
  end enumCodecInstance

  inline def derived[F[_], S, T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumCodecInstance[F, S, T, A](
      a => EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
    )(
      t => EnumDecoderDerivation.decodeEnum[F, T, A](t, configuration, stringDecoder, generic)(singletons)
    )(
      self => EnumDecoderDerivation.enumDecodersDict[F, T, A](self, configuration, generic)
    )
  end derived
  def unsafeDerived[F[_], S, T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): Codec[F, S, T, A] =
    enumCodecInstance[F, S, T, A](
      a => EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
    )(
      t => EnumDecoderDerivation.unsafeDecodeEnum[F, T, A, generic.Repr](t, configuration, stringDecoder, generic)
    )(
      self => EnumDecoderDerivation.enumDecodersDict[F, T, A](self, configuration, generic)
    )
  end unsafeDerived

  inline def derivedStringCodecEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Codec[F, String, String, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumCodecInstance[F, String, String, A](
      a => EnumEncoderDerivation.stringEncodeEnum(a, configuration, applicative, generic)
    )(
      t => EnumDecoderDerivation.stringDecodeEnum[F, A](t, configuration, generic)(singletons)
    )(
      self => EnumDecoderDerivation.enumDecodersDict[F, String, A](self, configuration, generic)
    )
  end derivedStringCodecEnum

  def unsafeDerivedStringCodecEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Codec[F, String, String, A] =
    enumCodecInstance[F, String, String, A](
      a => EnumEncoderDerivation.stringEncodeEnum(a, configuration, applicative, generic)
    )(
      t => EnumDecoderDerivation.unsafeStringDecodeEnum(t, configuration, generic)
    )(
      self => EnumDecoderDerivation.enumDecodersDict[F, String, A](self, configuration, generic)
    )
  end unsafeDerivedStringCodecEnum
end EnumCodecDerivation
object EnumCodecDerivation extends EnumCodecDerivation
