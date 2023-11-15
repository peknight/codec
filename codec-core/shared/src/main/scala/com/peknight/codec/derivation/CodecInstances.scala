package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.{Applicative, Monad}
import com.peknight.codec.configuration.{CodecConfiguration, DecoderConfiguration}
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Codec, Decoder, Encoder, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration

trait CodecInstances:
  def derived[F[_], S, O, T, E, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType.Aux[S, O],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    if generic.isProduct then
      derivedProduct[F, S, O, T, E, A](configuration, objectType, decodeObject, failure,
        encoders.asInstanceOf, decoders.asInstanceOf)
    else
      derivedSum[F, S, O, T, E, A](configuration, objectType, decodeObject, failure, stringEncoder,
        stringOptionDecoder, encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, O, T, E, A](
    configuration: CodecConfiguration,
    objectType0: ObjectType.Aux[S, O],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    new Codec.AsObject[F, S, T, E, A]:
      type Object = O
      protected def objectType: ObjectType.Aux[S, O] = objectType0
      def encodeObject(a: A): F[O] = EncoderDerivationInstances.encodeProduct(a, configuration, objectType0, encoders)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivationInstances.decodeProductEither(t, configuration, decodeObject, failure, decoders)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivationInstances.decodeProductValidatedNel(t, configuration, decodeObject, failure, decoders)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, O, T, E, A](
    configuration0: CodecConfiguration,
    objectType0: ObjectType.Aux[S, O],
    decodeObject: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    new Codec.AsObject[F, S, T, E, A] with SumEncoder[F, S, A] with SumDecoder[F, T, E, A]:
      type Object = O
      protected def objectType: ObjectType.Aux[S, O] = objectType0
      def configuration: DecoderConfiguration = configuration0
      def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A] = decoders0
      def encodeObject(a: A): F[O] = EncoderDerivationInstances.encodeSum(a, configuration0, objectType0, stringEncoder, encoders)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivationInstances.decodeSumEither(t, configuration0, decodeObject, failure, stringOptionDecoder, decoders0)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivationInstances.decodeSumValidatedNel(t, configuration0, decodeObject, failure, stringOptionDecoder, decoders0)
  end derivedSum
end CodecInstances
object CodecInstances extends CodecInstances
