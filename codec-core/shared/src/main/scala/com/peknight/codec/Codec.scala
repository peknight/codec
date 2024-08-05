package com.peknight.codec

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.derivation.CodecDerivation
import com.peknight.codec.error.{DecodingFailure, ParsingTypeError, WrongClassTag}
import com.peknight.codec.number.Number
import com.peknight.generic.tuple.Map
import com.peknight.generic.Generic
import com.peknight.codec.sum.{NullType, NumberType, ObjectType, StringType}

import scala.reflect.ClassTag
import scala.util.Try

trait Codec[F[_], S, T, A] extends Encoder[F, S, A] with Decoder[F, T, A]
object Codec extends CodecDerivation:
  def apply[F[_], S, T, A](using encoder: Encoder[F, S, A], decoder: Decoder[F, T, A]): Codec[F, S, T, A] =
    instance(encoder.encode)(decoder.decode)

  def instance[F[_], S, T, A](encode0: A => F[S])(decode0: T => F[Either[DecodingFailure, A]]): Codec[F, S, T, A] =
    new Codec[F, S, T, A]:
      def encode(a: A): F[S] = encode0(a)
      def decode(t: T): F[Either[DecodingFailure, A]] = decode0(t)

  def applicative[F[_]: Applicative, S, T, A](encode: A => S)(decode: T => Either[DecodingFailure, A])
  : Codec[F, S, T, A] =
    instance[F, S, T, A](a => encode(a).pure[F])(t => decode(t).pure[F])

  def map[F[_]: Applicative, S, T, A](encode: A => S)(decode: T => A): Codec[F, S, T, A] =
    applicative[F, S, T, A](encode)(t => decode(t).asRight[DecodingFailure])

  def mapOption[F[_]: Applicative, S, T, A: ClassTag](encode: A => S)(decode: T => Option[A]): Codec[F, S, T, A] =
    applicative[F, S, T, A](encode)(t => decode(t).toRight(WrongClassTag[A].value(t)))

  def mapTry[F[_]: Applicative, S, T, A: ClassTag](encode: A => S)(decode: T => Try[A]): Codec[F, S, T, A] =
    applicative[F, S, T, A](encode)(t => decode(t).toEither.left.map(e => ParsingTypeError[A](e).value(t)))

  def parse[F[_]: Applicative, S, T, A: ClassTag](encode: A => S)(decode: T => A): Codec[F, S, T, A] =
    mapTry(encode)(t => Try(decode(t)))

  def identity[F[_]: Applicative, A]: Codec[F, A, A, A] =
    map[F, A, A, A](Predef.identity)(Predef.identity)

  def const[F[_]: Applicative, S, T, A](encode: S)(decode: A): Codec[F, S, T, A] =
    map[F, S, T, A](_ => encode)(_ => decode)

  def cursor[F[_]: Applicative, S, A](encode0: A => F[S])(decode0: SuccessCursor[S] => F[Either[DecodingFailure, A]])
  : Codec[F, S, Cursor[S], A] =
    new Codec[F, S, Cursor[S], A]:
      def encode(a: A): F[S] = encode0(a)
      def decode(t: Cursor[S]): F[Either[DecodingFailure, A]] = t match
        case cursor: SuccessCursor[S] => decode0(cursor)
        case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft[A].pure[F]
  end cursor

  def cursorApplicative[F[_]: Applicative, S, A](encode: A => S)(decode: SuccessCursor[S] => Either[DecodingFailure, A])
  : Codec[F, S, Cursor[S], A] =
    cursor[F, S, A](a => encode(a).pure[F])(t => decode(t).pure[F])

  def cursorMap[F[_]: Applicative, S, A](encode: A => S)(decode: SuccessCursor[S] => A)
  : Codec[F, S, Cursor[S], A] =
    cursorApplicative[F, S, A](encode)(t => decode(t).asRight[DecodingFailure])

  def cursorIdentity[F[_]: Applicative, S]: Codec[F, S ,Cursor[S], S] =
    cursorMap[F, S, S](Predef.identity)(_.value)

  def cursorConst[F[_]: Applicative, S, A](encode: S)(decode: A): Codec[F, S, Cursor[S], A] =
    cursorMap[F, S, A](_ => encode)(_ => decode)

  def cursorValue[F[_]: Applicative, S, A](encode: A => F[S])(decode: S => F[Either[DecodingFailure, A]])
  : Codec[F, S, Cursor[S], A] =
    cursor[F, S, A](encode)(t => decode(t.value).map(_.left.map(_.cursor(t))))

  def cursorValueApplicative[F[_]: Applicative, S, A](encode: A => S)(decode: S => Either[DecodingFailure, A])
  : Codec[F, S, Cursor[S], A] =
    cursorApplicative(encode)(t => decode(t.value).left.map(_.cursor(t)))

  def cursorValueMap[F[_]: Applicative, S, A](encode: A => S)(decode: S => A): Codec[F, S, Cursor[S], A] =
    cursorMap[F, S, A](encode)(t => decode(t.value))

  def forProduct[F[_], S, A, Repr <: Tuple](labels: Map[Repr, [X] =>> String])
                                           (encode: A => Repr)(decode: Repr => Either[DecodingFailure, A])
                                           (using
                                             applicative: Applicative[F],
                                             objectType: ObjectType[S],
                                             nullType: NullType[S],
                                             encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], Repr],
                                             decoders: Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], Repr]
                                           ): Codec[F, S, Cursor[S], A] =
    cursor[F, S, A](a => Encoder.handleForProduct[F, S, A, Repr](a)(labels)(encode))(t => Decoder.handleForProduct[F, S, A, Repr](t)(labels)(decode))

  def forProductMap[F[_], S, A, Repr <: Tuple](labels: Map[Repr, [X] =>> String])
                                              (encode: A => Repr)(decode: Repr => A)
                                              (using
                                                applicative: Applicative[F],
                                                objectType: ObjectType[S],
                                                nullType: NullType[S],
                                                encoders: Generic.Product.Instances[[X] =>> Encoder[F, S, X], Repr],
                                                decoders: Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], Repr]
                                              ): Codec[F, S, Cursor[S], A] =
    forProduct[F, S, A, Repr](labels)(encode)(repr => decode(repr).asRight)

  def codecN[F[_], S, A](using Encoder[F, Number, A], Decoder[F, Number, A])
                        (using Applicative[F], NumberType[S], ClassTag[A]): Codec[F, S, Cursor[S], A] =
    Codec[F, S, Cursor[S], A](using Encoder.encodeN[F, S, A], Decoder.decodeN[F, S, A])

  def codecNS[F[_], S, A](using Encoder[F, Number, A], Decoder[F, Number, A])
                         (using Applicative[F], NumberType[S], StringType[S], ClassTag[A]): Codec[F, S, Cursor[S], A] =
    Codec[F, S, Cursor[S], A](using Encoder.encodeN[F, S, A], Decoder.decodeNS[F, S, A])

  def codecS[F[_], S, A](using Encoder[F, String, A], Decoder[F, String, A])
                        (using Applicative[F], StringType[S], ClassTag[A]): Codec[F, S, Cursor[S], A] =
    Codec[F, S, Cursor[S], A](using Encoder.encodeS[F, S, A], Decoder.decodeS[F, S, A])

  def stringCodecWithNumberDecoder[F[_], A](using Decoder[F, Number, A])(using Applicative[F])
  : Codec[F, String, String, A] =
    Codec[F, String, String, A](using Encoder.encodeWithToString[F, A], Decoder.stringDecodeWithNumberDecoder[F, A])

  def stringCodecWithNumberCodec[F[_], A](using Encoder[F, Number, A], Decoder[F, Number, A])(using Applicative[F])
  : Codec[F, String, String, A] =
    Codec[F, String, String, A](using Encoder.stringEncodeWithNumberEncoder[F, A], Decoder.stringDecodeWithNumberDecoder[F, A])
end Codec
