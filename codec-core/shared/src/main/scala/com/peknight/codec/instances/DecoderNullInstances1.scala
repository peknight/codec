package com.peknight.codec.instances

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, MissingField, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances1 extends DecoderNullInstances2:

  def handleDecodeOptionAO[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure, Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionAO

  given decodeOptionAO[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionAO[F, S, A](PartialFunction.empty)
  end decodeOptionAO

  def handleDecodeOptionOU[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure, Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionOU

  given decodeOptionOU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionOU[F, S, A](PartialFunction.empty)
  end decodeOptionOU

  def handleDecodeOptionAU[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure, Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionAU

  given decodeOptionAU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionAU[F, S, A](PartialFunction.empty)
  end decodeOptionAU

  given decodeNoneAO[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S], arrayType: ArrayType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocus => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneAO

  given decodeNoneOU[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S], nullType: NullType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneOU

  given decodeNoneAU[F[_], S](using applicative: Applicative[F], arrayType: ArrayType[S], nullType: NullType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneAU
end DecoderNullInstances1
