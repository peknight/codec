package com.peknight.codec.derivation

import cats.{Monad, Show}
import com.peknight.codec.config.CodecConfig
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.generic.Generic
import com.peknight.generic.tuple.syntax.sequence

trait CodecDerivation:
  def derived[F[_], S, A](using config: CodecConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    generic: Generic[A],
    encoders: => Generic.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    if generic.isProduct then
      derivedProduct[F, S, A](using config)(using monad, objectType, nullType, show, encoders.asInstanceOf,
        decoders.asInstanceOf)
    else
      derivedSum[F, S, A](using config)(using monad, objectType, stringEncoder, stringDecoder,
        stringOptionDecoder, show, generic.asInstanceOf, encoders.asInstanceOf, decoders.asInstanceOf)

  def derivedProduct[F[_], S, A](using config: CodecConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    show: Show[S],
    encoders: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A],
    decoders: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    new Codec[F, S, Cursor[S], A]:
      def encode(a: A): F[S] = EncoderDerivation.encodeProduct(a, config, objectType, encoders)
      def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
        DecoderDerivation.decodeProduct(cursor, config, objectType, nullType, show, decoders)
  end derivedProduct

  def derivedSum[F[_], S, A](using config: CodecConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    generic: Generic.Sum[A],
    encoders: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A],
    decoders0: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Codec[F, S, Cursor[S], A] =
    generic.singletons.sequence match
      case Some(singletons) =>
        new Codec[F, S, Cursor[S], A] with EnumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            EnumDecoderDerivation.enumDecodersDict[F, Cursor[S], A](this, config, generic)
          def encode(a: A): F[S] = EnumEncoderDerivation.encodeEnum(a, config, stringEncoder, generic)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnum(cursor, config, stringDecoder, generic)(singletons)(_.cursor(_))
      case _ =>
        new Codec[F, S, Cursor[S], A] with SumEncoder[F, S, A]
          with SumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            DecoderDerivation.decodersDict[F, Cursor[S], A](config, decoders0)
          def encode(a: A): F[S] =
            EncoderDerivation.encodeSum(a, config, objectType, stringEncoder, encoders)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            DecoderDerivation.decodeSum(cursor, config, objectType, stringOptionDecoder, show, decoders0)
  end derivedSum
end CodecDerivation
object CodecDerivation extends CodecDerivation
