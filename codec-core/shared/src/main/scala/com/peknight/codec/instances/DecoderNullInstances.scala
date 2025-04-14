package com.peknight.codec.instances

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Applicative, Functor, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, MissingField, NotNull}
import com.peknight.codec.reader.Key
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances extends DecoderNullInstances1:
  def handleDecodeOptionAOU[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure,Option[A]]]] = PartialFunction.empty)(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionAOU

  given decodeOptionAOU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionAOU[F, S, A]()
  end decodeOptionAOU

  given decodeOptionKey[F[_], A](using monad: Functor[F], decoder: Decoder[F, Key, A]): Decoder[F, Key, Option[A]] =
    Decoder.instance[F, Key, Option[A]] { key =>
      decoder.decode(key).map {
        case Right(value) => value.some.asRight
        case Left(error) if DecodingFailure.isNullError(error) => none.asRight
        case Left(error) => error.asLeft
      }
    }

  given decodeSome[F[_], T, A](using functor: Functor[F], decoder: Decoder[F, T, A]): Decoder[F, T, Some[A]] =
    Decoder.instance[F, T, Some[A]](t => decoder.decode(t).map(_.map(Some(_))))

  given decodeNoneAOU[F[_], S](
    using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneAOU
end DecoderNullInstances
