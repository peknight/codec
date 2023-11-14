package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.{Applicative, Monad}
import com.peknight.codec.configuration.{CodecConfiguration, DecoderConfiguration}
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration

trait CodecInstances:
  def derived[F[_], S, T, E, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    encodeObject: EncodeObjectOps[S],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    if generic.isProduct then
      derivedProduct[F, S, T, E, A](configuration, encodeObject, decodeObject, failure,
        encoders.asInstanceOf, decoders.asInstanceOf)
    else
      derivedSum[F, S, T, E, A](configuration, encodeObject, decodeObject, failure, stringEncoder, stringOptionDecoder,
        encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, T, E, A](
    configuration: CodecConfiguration,
    encodeObject: EncodeObjectOps[S],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    new Codec[F, S, T, E, A]:
      def encode(a: A): F[S] = EncoderDerivationInstances.encodeProduct(a, configuration, encodeObject, encoders)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivationInstances.decodeProductEither(t, configuration, decodeObject, failure, decoders)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivationInstances.decodeProductValidatedNel(t, configuration, decodeObject, failure, decoders)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, T, E, A](
    configuration0: CodecConfiguration,
    encodeObject: EncodeObjectOps[S],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    new Codec[F, S, T, E, A] with SumEncoder[F, S, A] with SumDecoder[F, T, E, A]:
      def configuration: DecoderConfiguration = configuration0
      def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A] = decoders0
      def encode(a: A): F[S] = EncoderDerivationInstances.encodeSum(a, configuration0, encodeObject, stringEncoder, encoders)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivationInstances.decodeSumEither(t, configuration0, decodeObject, failure, stringOptionDecoder, decoders0)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivationInstances.decodeSumValidatedNel(t, configuration0, decodeObject, failure, stringOptionDecoder, decoders0)
  end derivedSum
end CodecInstances
object CodecInstances extends CodecInstances
