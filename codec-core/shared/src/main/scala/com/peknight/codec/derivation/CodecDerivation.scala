package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.syntax.functor.*
import cats.{Applicative, Monad}
import com.peknight.codec.*
import com.peknight.codec.configuration.{CodecConfiguration, DecoderConfiguration}
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration

trait CodecDerivation:
  def derived[F[_], S, O, T, E, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    if generic.isProduct then
      derivedProduct[F, S, O, T, E, A](configuration, cursorType, objectType, failure, encoders.asInstanceOf,
        decoders.asInstanceOf)
    else
      derivedSum[F, S, O, T, E, A](configuration, cursorType, objectType, failure, stringEncoder, stringOptionDecoder,
        encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, O, T, E, A](
    configuration: CodecConfiguration,
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    new Codec.AsObject[F, S, T, E, A]:
      type Object = O
      def encodeObject(a: A): F[O] = EncoderDerivation.encodeProduct(a, configuration, objectType, encoders)
      def encode(a: A): F[S] = encodeObject(a).map(objectType.to)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivation.decodeProductEither(t, configuration, cursorType, objectType, failure, decoders)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivation.decodeProductValidatedNel(t, configuration, cursorType, objectType, failure,
          decoders)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, O, T, E, A](
    configuration0: CodecConfiguration,
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec.AsObject.Aux[F, S, O, T, E, A] =
    new Codec.AsObject[F, S, T, E, A] with SumEncoder[F, S, A] with SumDecoder[F, T, E, A]:
      type Object = O
      def configuration: DecoderConfiguration = configuration0
      def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A] = decoders0
      def encodeObject(a: A): F[O] =
        EncoderDerivation.encodeSum(a, configuration0, objectType, stringEncoder, encoders)
      def encode(a: A): F[S] = encodeObject(a).map(objectType.to)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivation.decodeSumEither(t, configuration0, cursorType, objectType, failure,
          stringOptionDecoder, decoders0)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivation.decodeSumValidatedNel(t, configuration0, cursorType, objectType, failure,
          stringOptionDecoder, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
