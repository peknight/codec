package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.syntax.functor.*
import cats.{Applicative, Monad}
import com.peknight.codec.*
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.tuple.syntax.sequence

trait CodecDerivation:
  def derived[F[_], S, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], DecodingFailure, String],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Codec[F, S, Cursor[S], DecodingFailure, A] =
    if generic.isProduct then
      derivedProduct[F, S, A](configuration, objectType, nullType, encoders.asInstanceOf, decoders.asInstanceOf)
    else
      derivedSum[F, S, A](configuration, objectType, stringEncoder, stringDecoder, stringOptionDecoder,
        generic.asInstanceOf, encoders.asInstanceOf, decoders.asInstanceOf)

  private[this] def derivedProduct[F[_]: Applicative, S, A](
    configuration: CodecConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Codec[F, S, Cursor[S], DecodingFailure, A] =
    new Codec[F, S, Cursor[S], DecodingFailure, A]:
      def encode(a: A): F[S] = EncoderDerivation.encodeProduct(a, configuration, objectType, encoders)
      def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
        DecoderDerivation.decodeProductEither(cursor, configuration, objectType, nullType, decoders)
      def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
        DecoderDerivation.decodeProductValidatedNel(cursor, configuration, objectType, nullType, decoders)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, A](
    configuration: CodecConfiguration,
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], DecodingFailure, String],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    generic: Generic.Sum[A],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Codec[F, S, Cursor[S], DecodingFailure, A] =
    generic.singletons.sequence match
      case Some(singletons) =>
        new Codec[F, S, Cursor[S], DecodingFailure, A] with EnumDecoder[F, Cursor[S], DecodingFailure, A]:
          def decoders: Map[String, Decoder[F, Cursor[S], DecodingFailure, _]] =
            EnumDecoderDerivation.enumDecodersDict[F, Cursor[S], DecodingFailure, A](this, configuration, generic)
          def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnumEither(cursor, configuration, stringDecoder, generic, singletons)
          def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnumValidatedNel(cursor, configuration, stringDecoder, generic, singletons)
      case _ =>
        new Codec[F, S, Cursor[S], DecodingFailure, A] with SumEncoder[F, S, A]
          with SumDecoder[F, Cursor[S], DecodingFailure, A]:
          def decoders: Map[String, Decoder[F, Cursor[S], DecodingFailure, _]] =
            DecoderDerivation.decodersDict[F, Cursor[S], DecodingFailure, A](configuration, decoders0)
          def encode(a: A): F[S] =
            EncoderDerivation.encodeSum(a, configuration, objectType, stringEncoder, encoders)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            DecoderDerivation.decodeSumEither(cursor, configuration, objectType, stringOptionDecoder, decoders0)
          def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
            DecoderDerivation.decodeSumValidatedNel(cursor, configuration, objectType, stringOptionDecoder, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
