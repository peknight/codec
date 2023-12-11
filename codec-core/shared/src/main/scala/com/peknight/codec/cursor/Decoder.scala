package com.peknight.codec.cursor

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.DecodingFailure

object Decoder:
  def instance[F[_]: Applicative, S, A](f: SuccessCursor[S] => F[Either[DecodingFailure[Cursor[S]], A]])
  : Decoder[F, S, A] =
    new Decoder[F, S, A]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure[Cursor[S]], A]] =
        t match
          case cursor: SuccessCursor[S] => f(cursor)
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft[A].pure[F]
      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure[Cursor[S]], A]] =
        t match
          case cursor: SuccessCursor[S] => f(cursor).map(_.toValidatedNel)
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel[A].pure[F]
  end instance
end Decoder