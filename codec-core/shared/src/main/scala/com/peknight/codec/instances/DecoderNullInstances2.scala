package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.{Cursor, CursorDecoder, FailedCursor, SuccessCursor}
import com.peknight.codec.error.{DecodingFailure, MissingField, NotNull}
import com.peknight.codec.sum.{ArrayType, NullType, ObjectType}

trait DecoderNullInstances2:
  given decodeOptionO[F[_], S, A](using applicative: Applicative[F], decoder: CursorDecoder[F, S, A],
                                   objectType: ObjectType[S]): CursorDecoder[F, S, Option[A]] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], Option[A]] {
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeOptionO

  given decodeOptionA[F[_], S, A](using applicative: Applicative[F], decoder: CursorDecoder[F, S, A],
                                  arrayType: ArrayType[S]): CursorDecoder[F, S, Option[A]] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], Option[A]] {
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => none.asRight.pure
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeOptionA

  given decodeOptionN[F[_], S, A](using applicative: Applicative[F], decoder: CursorDecoder[F, S, A],
                                  nullType: NullType[S]): CursorDecoder[F, S, Option[A]] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], Option[A]] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => none.asRight.pure
      case cursor: SuccessCursor[S] => decoder.decode(cursor).map(_.map(_.some))
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeOptionN

  given decodeNoneO[F[_], S](using applicative: Applicative[F], objectType: ObjectType[S])
  : CursorDecoder[F, S, None.type] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], None.type] {
      case cursor: SuccessCursor[S] => NotNull(cursor).asLeft.pure
      case cursor: FailedCursor[S] if !cursor.incorrectFocusO => None.asRight.pure
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeNoneO

  given decodeNoneA[F[_], S](using applicative: Applicative[F], arrayType: ArrayType[S])
  : CursorDecoder[F, S, None.type] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], None.type] {
      case cursor: SuccessCursor[S] => NotNull(cursor).asLeft.pure
      case cursor: FailedCursor[S] if !cursor.incorrectFocusA => None.asRight.pure
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeNoneA

  given decodeNoneN[F[_], S](using applicative: Applicative[F], nullType: NullType[S])
  : CursorDecoder[F, S, None.type] =
    Decoder.instance[F, Cursor[S], DecodingFailure[Cursor[S]], None.type] {
      case cursor: SuccessCursor[S] if nullType.isNull(cursor.value) => None.asRight.pure
      case cursor: SuccessCursor[S] => NotNull(cursor).asLeft.pure
      case cursor: FailedCursor[S] => MissingField(cursor).asLeft.pure
    }
  end decodeNoneN
end DecoderNullInstances2
