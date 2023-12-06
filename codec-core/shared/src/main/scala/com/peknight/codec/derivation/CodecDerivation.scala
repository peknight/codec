package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.syntax.functor.*
import cats.{Applicative, Monad}
import com.peknight.codec.*
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.cursor.CursorType
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.tuple.syntax.sequence

trait CodecDerivation:
  def derived[F[_], S, O, T, E, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    nullType: NullType[S],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, E, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    if generic.isProduct then
      derivedProduct[F, S, O, T, E, A](configuration, cursorType, objectType, nullType, failure, encoders.asInstanceOf,
        decoders.asInstanceOf)
    else
      derivedSum[F, S, O, T, E, A](configuration, cursorType, objectType, failure, stringEncoder, stringDecoder,
        stringOptionDecoder, generic.asInstanceOf, encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, O, T, E, A](
    configuration: CodecConfiguration,
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    nullType: NullType[S],
    failure: Migration[DecodingFailure[T], E],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    new Codec[F, S, T, E, A]:
      def encode(a: A): F[S] = EncoderDerivation.encodeProduct(a, configuration, objectType, encoders)
      def decode(t: T): F[Either[E, A]] =
        DecoderDerivation.decodeProductEither(t, configuration, cursorType, objectType, nullType, failure, decoders)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        DecoderDerivation.decodeProductValidatedNel(t, configuration, cursorType, objectType, nullType, failure,
          decoders)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, O, T, E, A](
    configuration: CodecConfiguration,
    cursorType: CursorType.Aux[T, S],
    objectType: ObjectType.Aux[S, O],
    failure: Migration[DecodingFailure[T], E],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, T, E, String],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    generic: Generic.Sum[A],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Codec[F, S, T, E, A] =
    generic.singletons.sequence match
      case Some(singletons) =>
        new Codec[F, S, T, E, A] with EnumDecoder[F, T, E, A]:
          def decoders: Map[String, Decoder[F, T, E, _]] =
            EnumDecoderDerivation.enumDecodersDict[F, T, E, A](this, configuration, generic)
          def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
          def decode(t: T): F[Either[E, A]] =
            EnumDecoderDerivation.decodeEnumEither(t, configuration, failure, stringDecoder, generic, singletons)
          def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
            EnumDecoderDerivation.decodeEnumValidatedNel(t, configuration, failure, stringDecoder, generic, singletons)
      case _ =>
        new Codec[F, S, T, E, A] with SumEncoder[F, S, A] with SumDecoder[F, T, E, A]:
          def decoders: Map[String, Decoder[F, T, E, _]] =
            DecoderDerivation.decodersDict[F, T, E, A](configuration, decoders0)
          def encode(a: A): F[S] = EncoderDerivation.encodeSum(a, configuration, objectType, stringEncoder, encoders)
          def decode(t: T): F[Either[E, A]] =
            DecoderDerivation.decodeSumEither(t, configuration, cursorType, objectType, failure,
              stringOptionDecoder, decoders0)
          def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
            DecoderDerivation.decodeSumValidatedNel(t, configuration, cursorType, objectType, failure,
              stringOptionDecoder, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
