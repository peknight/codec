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

trait DecoderNullInstances2:
  def handleDecodeOptionO[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure,Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionO

  given decodeOptionO[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionO[F, S, A](PartialFunction.empty)
  end decodeOptionO

  def handleDecodeOptionA[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure,Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionA

  given decodeOptionA[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionA[F, S, A](PartialFunction.empty)
  end decodeOptionA

  def handleDecodeOptionU[F[_], S, A](f: PartialFunction[S, F[Either[DecodingFailure,Option[A]]]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    given Show[S] = Show.fromToString
    Decoder.instance[F, Cursor[S], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] if f.isDefinedAt(cursor.value) => f(cursor.value).map(_.left.map(_.cursor(cursor)))
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft.pure
    }
  end handleDecodeOptionU

  given decodeOptionU[F[_], S, A](
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    nullType: NullType[S]
  ): Decoder[F, Cursor[S], Option[A]] =
    handleDecodeOptionU[F, S, A](PartialFunction.empty)
  end decodeOptionU

  given decodeNoneO[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneO

  given decodeNoneA[F[_], S](using applicative: Applicative[F], arrayType: ArrayType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => None.asRight
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneA

  given decodeNoneU[F[_], S](using applicative: Applicative[F], nullType: NullType[S])
  : Decoder[F, Cursor[S], None.type] =
    given Show[S] = Show.fromToString
    Decoder.applicative[F, Cursor[S], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight
      case cursor: SuccessCursor[S] => NotNull.cursor(cursor).asLeft
      case cursor: FailedCursor[S] => MissingField.cursor(cursor).asLeft
    }
  end decodeNoneU
end DecoderNullInstances2
