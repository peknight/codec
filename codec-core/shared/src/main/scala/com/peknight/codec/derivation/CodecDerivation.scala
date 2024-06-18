package com.peknight.codec.derivation

import cats.{Applicative, Monad}
import com.peknight.codec.configuration.CodecConfiguration
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic
import com.peknight.generic.tuple.syntax.sequence

trait CodecDerivation:
  def derived[F[_], S, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    if generic.isProduct then
      derivedProduct[F, S, A](using configuration)(using monad, objectType, nullType, encoders.asInstanceOf,
        decoders.asInstanceOf)
    else
      derivedSum[F, S, A](using configuration)(using monad, objectType, stringEncoder, stringDecoder,
        stringOptionDecoder, generic.asInstanceOf, encoders.asInstanceOf, decoders.asInstanceOf)

  def derivedProduct[F[_], S, A](using configuration: CodecConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    new Codec[F, S, Cursor[S], A]:
      def encode(a: A): F[S] = EncoderDerivation.encodeProduct(a, configuration, objectType, encoders)
      def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
        DecoderDerivation.decodeProduct(cursor, configuration, objectType, nullType, decoders)
  end derivedProduct

  def derivedSum[F[_], S, A](using configuration: CodecConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    generic: Generic.Sum[A],
    encoders: Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    generic.singletons.sequence match
      case Some(singletons) =>
        new Codec[F, S, Cursor[S], A] with EnumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            EnumDecoderDerivation.enumDecodersDict[F, Cursor[S], A](this, configuration, generic)
          def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, generic)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnum(cursor, configuration, stringDecoder, generic)(singletons)
      case _ =>
        new Codec[F, S, Cursor[S], A] with SumEncoder[F, S, A]
          with SumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            DecoderDerivation.decodersDict[F, Cursor[S], A](configuration, decoders0)
          def encode(a: A): F[S] =
            EncoderDerivation.encodeSum(a, configuration, objectType, stringEncoder, encoders)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            DecoderDerivation.decodeSum(cursor, configuration, objectType, stringOptionDecoder, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
