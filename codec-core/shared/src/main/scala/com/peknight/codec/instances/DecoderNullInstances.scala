package com.peknight.codec.instances

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Applicative, Functor, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{MissingField, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances extends DecoderNullInstances1:
  given decodeOptionAOU[F[_], S, A](
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
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeOptionAOU

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
