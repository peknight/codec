package com.peknight.codec.cursor

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.validated.*
import com.peknight.codec.error.DecodingFailure

trait Decoder[F[_]: Applicative, S, A] extends com.peknight.codec.Decoder[F, Cursor[S], DecodingFailure[Cursor[S]], A]:
  def apply(t: SuccessCursor[S]): F[Either[DecodingFailure[Cursor[S]], A]]
  def applyAccumulating(t: SuccessCursor[S]): F[ValidatedNel[DecodingFailure[Cursor[S]], A]] =
    apply(t).map {
      case Right(a) => a.validNel[DecodingFailure[Cursor[S]]]
      case Left(e) => e.invalidNel[A]
    }
  def decode(t: Cursor[S]): F[Either[DecodingFailure[Cursor[S]], A]] =
    t match
      case cursor: SuccessCursor[S] => apply(cursor)
      case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft[A].pure[F]
  def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure[Cursor[S]], A]] =
    t match
      case cursor: SuccessCursor[S] => applyAccumulating(cursor)
      case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel[A].pure[F]
  def decodeS(s: S): F[Either[DecodingFailure[Cursor[S]], A]] = apply(Cursor.from(s))
end Decoder
