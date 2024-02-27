package com.peknight.codec.instances

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Applicative, Functor}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, MissingField, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances extends DecoderNullInstances1:
  given decodeOption[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], DecodingFailure, Option[A]] =
    Decoder.instance[F, Cursor[S], DecodingFailure, Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeOption

  given decodeSome[F[_], T, E, A](using functor: Functor[F], decoder: Decoder[F, T, E, A]): Decoder[F, T, E, Some[A]] =
    Decoder.instance[F, T, E, Some[A]](t => decoder.decode(t).map(_.map(Some(_))))

  given decodeNone[F[_], S](
    using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], DecodingFailure, None.type] =
    Decoder.instance[F, Cursor[S], DecodingFailure, None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight.pure
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft.pure
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => None.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeNone
end DecoderNullInstances
