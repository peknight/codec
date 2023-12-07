package com.peknight.codec.syntax

import cats.Functor
import cats.data.{Validated, ValidatedNel}
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.id.{Decoder, Encoder}

trait DecoderSyntax:
  extension [F[_], T] (ft: F[T])
    def decodeTo[A](using functor: Functor[F], encoder: Encoder[A, T]): F[A] = ft.map(encoder.encode)
    def decodeTo[A](f: F[T] => (T => A) => F[A])(using encoder: Encoder[A, T]): F[A] = f(ft)(encoder.encode)
    def eitherDecodeTo[E, A](f: F[T] => (T => Either[E, A]) => F[A])(using decoder: Decoder[T, E, A]): F[A] =
      f(ft)(decoder.decode)
    def validatedDecodeTo[E, A](f: F[T] => (T => Validated[E, A]) => F[A])(using decoder: Decoder[T, E, A]): F[A] =
      f(ft)(t => decoder.decode(t).toValidated)
    def validatedNelDecodeTo[E, A](f: F[T] => (T => ValidatedNel[E, A]) => F[A])(using decoder: Decoder[T, E, A]): F[A] =
      f(ft)(t => decoder.decodeAccumulating(t))
  end extension
end DecoderSyntax
object DecoderSyntax extends DecoderSyntax