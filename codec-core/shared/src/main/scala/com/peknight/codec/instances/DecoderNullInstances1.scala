package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{MissingField, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances1 extends DecoderNullInstances2:
  given decodeOptionAO[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeOptionAO

  given decodeOptionOU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeOptionOU

  given decodeOptionAU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end decodeOptionAU

  given decodeNoneAO[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S], arrayType: ArrayType[S])
  : Decoder[F, Cursor[S], None.type] =
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneAO

  given decodeNoneOU[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S], nullType: NullType[S])
  : Decoder[F, Cursor[S], None.type] =
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneOU

  given decodeNoneAU[F[_], S](using applicative: Applicative[F], arrayType: ArrayType[S], nullType: NullType[S])
  : Decoder[F, Cursor[S], None.type] =
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneAU
end DecoderNullInstances1
