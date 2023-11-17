package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.syntax.functor.*
import cats.{Applicative, Monad}
import com.peknight.codec.*
import com.peknight.codec.configuration.{CodecConfiguration, DecoderConfiguration}
import com.peknight.codec.error.DecodingFailure
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.tuple.syntax.sequence

trait CodecDerivation:
  def derived[F[_], S, O, T, E, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, E, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    if generic.isProduct then
      derivedProduct[F, S, O, T, E, A](configuration, cursorType, objectType, failure, encoders.asInstanceOf,
        decoders.asInstanceOf)
    else
      derivedSum[F, S, O, T, E, A](configuration, cursorType, objectType, failure, stringEncoder, stringDecoder,
        stringOptionDecoder, generic.asInstanceOf, encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, O, T, E, A](
    configuration: CodecConfiguration,
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    new Codec[F, S, T, E, A]:
      def encode(a: A): F[S] = EncoderDerivation.encodeProduct(a, configuration, objectType, encoders)
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
    stringDecoder: Decoder[F, T, E, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic0: Generic.Sum[A],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    generic0.singletons.sequence match
      case Some(singletons) =>
        new Codec[F, S, T, E, A] with EnumDecoder[F, T, E, A]:
          def generic: Generic.Sum[A] = generic0
          def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration0, stringEncoder, generic0)
          def decode(t: T): F[Either[E, A]] =
            EnumDecoderDerivation.decodeEnumEither(t, configuration0, failure, stringDecoder, generic0, singletons)
          def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
            EnumDecoderDerivation.decodeEnumValidatedNel(t, configuration0, failure, stringDecoder, generic0, singletons)
      case _ =>
        new Codec[F, S, T, E, A] with SumEncoder[F, S, A] with SumDecoder[F, T, E, A]:
          def configuration: DecoderConfiguration = configuration0
          def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A] = decoders0
          def encode(a: A): F[S] = EncoderDerivation.encodeSum(a, configuration0, objectType, stringEncoder, encoders)
          def decode(t: T): F[Either[E, A]] =
            DecoderDerivation.decodeSumEither(t, configuration0, cursorType, objectType, failure,
              stringOptionDecoder, decoders0)
          def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
            DecoderDerivation.decodeSumValidatedNel(t, configuration0, cursorType, objectType, failure,
              stringOptionDecoder, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
