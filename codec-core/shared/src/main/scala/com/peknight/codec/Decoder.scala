package com.peknight.codec

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import cats.{Functor, Monad}
import com.peknight.codec.derivation.DecoderDerivationInstances
import com.peknight.codec.instances.*

trait Decoder[F[_], T, E, A]:
  self =>
  def decode(t: T): F[Either[E, A]]
  def decodeAccumulating(t: T): F[ValidatedNel[E, A]]
  def map[B](f: A => B)(using Functor[F]): Decoder[F, T, E, B] =
    new Decoder[F, T, E, B]:
      def decode(t: T): F[Either[E, B]] = self.decode(t).map(_.map(f))
      def decodeAccumulating(t: T): F[ValidatedNel[E, B]] = self.decodeAccumulating(t).map(_.map(f))
  end map
  def flatMap[B](f: A => Decoder[F, T, E, B])(using Monad[F]): Decoder[F, T, E, B] =
    new Decoder[F, T, E, B]:
      def decode(t: T): F[Either[E, B]] = self.decode(t).flatMap {
        case Right(a) => f(a).decode(t)
        case Left(e) => e.asLeft[B].pure[F]
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, B]] = self.decodeAccumulating(t).flatMap {
        case Valid(a) => f(a).decodeAccumulating(t)
        case Invalid(e) => e.invalid[B].pure[F]
      }
  end flatMap
  def mapError[EE](f: E => EE)(using Functor[F]): Decoder[F, T, EE, A] =
    new Decoder[F, T, EE, A]:
      def decode(t: T): F[Either[EE, A]] = self.decode(t).map(_.left.map(f))
      def decodeAccumulating(t: T): F[ValidatedNel[EE, A]] = self.decodeAccumulating(t).map(_.leftMap(_.map(f)))
  end mapError
end Decoder
object Decoder extends DecoderEitherMigrationInstances
  with DecoderValidatedMigrationInstances
  with DecoderValidatedNelMigrationInstances
  with DecoderMigrationInstances
  with DecoderErrorMigrationInstances
  with DecoderDerivationInstances
  with DecoderLowPriorityInstances