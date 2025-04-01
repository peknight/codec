package com.peknight.codec.syntax

import cats.syntax.functor.*
import cats.{Functor, Id}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.{Decoder, Encoder}

trait DecoderSyntax:
  extension [F[_], T] (ft: F[T])
    def decodeTo[A](using functor: Functor[F], encoder: Encoder[Id, A, T]): F[A] = ft.map(encoder.encode)
    def decodeTo[A](f: F[T] => (T => A) => F[A])(using encoder: Encoder[Id, A, T]): F[A] = f(ft)(encoder.encode)
    def eitherDecodeTo[A](f: F[T] => (T => Either[DecodingFailure, A]) => F[A])(using decoder: Decoder[Id, T, A]): F[A] =
      f(ft)(decoder.decode)
  end extension

  extension [T] (t: T)
    def asA[F[_], A](using decoder: Decoder[F, T, A]): F[Either[DecodingFailure, A]] = decoder.decode(t)
  end extension

  extension [S] (s: S)
    def cursorAsA[F[_], A](using decoder: Decoder[F, Cursor[S], A]): F[Either[DecodingFailure, A]] = decoder.decodeS(s)
  end extension
end DecoderSyntax
object DecoderSyntax extends DecoderSyntax