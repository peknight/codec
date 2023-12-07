package com.peknight.codec

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, StateT, ValidatedNel}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import cats.{Applicative, Apply, Functor, Monad}
import com.peknight.cats.ext.instances.eitherT.given
import com.peknight.codec.cursor.CursorType
import com.peknight.codec.instances.*
import com.peknight.generic.priority.PriorityInstancesF3

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
  def handleErrorWith(f: E => Decoder[F, T, E, A])(using Monad[F]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = self.decode(t).flatMap {
        case Left(e) => f(e).decode(t)
        case right => right.pure[F]
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = self.decodeAccumulating(t).flatMap {
        case Invalid(e) => f(e.head).decodeAccumulating(t)
        case valid => valid.pure[F]
      }
  end handleErrorWith
  def ensure(pred: A => Boolean, error: T => E)(using Functor[F]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = self.decode(t).map {
        case r @ Right(a) => if pred(a) then r else error(t).asLeft[A]
        case left => left
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = self.decodeAccumulating(t).map {
        case v @ Valid(a) => if pred(a) then v else error(t).invalidNel[A]
        case invalid => invalid
      }
  end ensure
  def ensure(errors: A => T => List[E])(using Functor[F]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = self.decode(t).map {
        case r @ Right(a) => errors(a)(t) match
          case Nil => r
          case error :: _ => error.asLeft[A]
        case left => left
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = self.decodeAccumulating(t).map {
        case v@Valid(a) => errors(a)(t) match
          case Nil => v
          case h :: t => NonEmptyList(h, t).invalid[A]
        case invalid => invalid
      }
  end ensure
  def validate(pred: T => Boolean, error: T => E)(using Applicative[F]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = if pred(t) then self.decode(t) else error(t).asLeft[A].pure[F]
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        if pred(t) then self.decodeAccumulating(t)
        else self.decodeAccumulating(t).map(_.fold(
          nel => NonEmptyList(error(t), nel.toList),
          _ => NonEmptyList.one(error(t))
        ).invalid[A])
  end validate
  def validate(errors: T => List[E])(using Applicative[F]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = errors(t) match
        case Nil => self.decode(t)
        case error :: _ => error.asLeft[A].pure[F]
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = errors(t) match
        case Nil => self.decodeAccumulating(t)
        case head :: tail =>
          val nel = NonEmptyList(head, tail)
          self.decodeAccumulating(t).map(_.fold(nel ::: _, _ => nel).invalid[A])
  end validate
  def product[B](fb: Decoder[F, T, E, B])(using Apply[F]): Decoder[F, T, E, (A, B)] =
    new Decoder[F, T, E, (A, B)]:
      def decode(t: T): F[Either[E, (A, B)]] =
        (self.decode(t), fb.decode(t)).mapN { (ra, rb) =>
          for
            a <- ra
            b <- rb
          yield (a, b)
        }
      def decodeAccumulating(t: T): F[ValidatedNel[E, (A, B)]] =
        (self.decodeAccumulating(t), fb.decodeAccumulating(t)).mapN((ra, rb) => (ra, rb).mapN((_, _)))
  end product
  def or[AA >: A](d: => Decoder[F, T, E, AA])(using Monad[F]): Decoder[F, T, E, AA] =
    new Decoder[F, T, E, AA]:
      def decode(t: T): F[Either[E, AA]] = self.decode(t).flatMap {
        case Left(_) => d.decode(t)
        case right => right.pure[F]
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, AA]] = self.decodeAccumulating(t).flatMap {
        case Invalid(_) => d.decodeAccumulating(t)
        case valid => valid.pure[F]
      }
  end or
  def either[B](decodeB: Decoder[F, T, E, B])(using Monad[F]): Decoder[F, T, E, Either[A, B]] =
    new Decoder[F, T, E, Either[A, B]]:
      def decode(t: T): F[Either[E, Either[A, B]]] = self.decode(t).flatMap {
        case Right(a) => a.asLeft[B].asRight[E].pure[F]
        case _ => decodeB.decode(t).map {
          case Right(b) => b.asRight[A].asRight[E]
          case left => left.asInstanceOf[Either[E, Either[A, B]]]
        }
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, Either[A, B]]] = self.decodeAccumulating(t).flatMap {
        case Valid(a) => a.asLeft[B].validNel[E].pure[F]
        case _ => decodeB.decodeAccumulating(t).map {
          case Valid(b) => b.asRight[A].validNel[E]
          case invalid => invalid.asInstanceOf[ValidatedNel[E, Either[A, B]]]
        }
      }
  end either
  def prepare(f: T => T): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = self.decode(f(t))
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = self.decodeAccumulating(f(t))
  end prepare
  def at(field: String)(using cursorType: CursorType[T]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = self.decode(cursorType.downField(t, field))
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = self.decodeAccumulating(cursorType.downField(t, field))
  end at
  def emap[B](f: A => T => Either[E, B])(using Functor[F]): Decoder[F, T, E, B] =
    new Decoder[F, T, E, B]:
      def decode(t: T): F[Either[E, B]] = self.decode(t).map {
        case Right(a) => f(a)(t)
        case Left(e) => e.asLeft[B]
      }
      def decodeAccumulating(t: T): F[ValidatedNel[E, B]] = self.decodeAccumulating(t).map {
        case Valid(a) => f(a)(t).toValidatedNel
        case Invalid(e) => e.invalid[B]
      }
  end emap
  def mapError[EE](f: E => EE)(using Functor[F]): Decoder[F, T, EE, A] =
    new Decoder[F, T, EE, A]:
      def decode(t: T): F[Either[EE, A]] = self.decode(t).map(_.left.map(f))
      def decodeAccumulating(t: T): F[ValidatedNel[EE, A]] = self.decodeAccumulating(t).map(_.leftMap(_.map(f)))
  end mapError
end Decoder
object Decoder extends DecoderCursorInstances
  with DecoderStringInstances
  with DecoderVectorInstances
  with DecoderObjectInstances
  with DecoderNullInstances
  with DecoderMigrationInstances
  with DecoderDerivationInstances
  with PriorityInstancesF3[Decoder]:
  def apply[F[_], T, E, A](using decoder: Decoder[F, T, E, A]): Decoder[F, T, E, A] = decoder
  def const[F[_]: Applicative, T, E, A](a: A): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = a.asRight[E].pure[F]
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = a.validNel[E].pure[F]
  end const
  def instance[F[_]: Functor, T, E, A](f: T => F[Either[E, A]]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = f(t)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = f(t).map(_.toValidatedNel)
  end instance
  def fromState[F[_]: Monad, T, E, A](s: StateT[[X] =>> F[Either[E, X]], T, A]): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = s.runA(t)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = s.runA(t).map(_.toValidatedNel)
  end fromState
  def failed[F[_]: Applicative, T, E, A](failure: E): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = failure.asLeft[A].pure[F]
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = failure.invalidNel[A].pure[F]
  end failed
  def failed[F[_] : Applicative, T, E, A](f: T => E): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = f(t).asLeft[A].pure[F]
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] = f(t).invalidNel[A].pure[F]
  end failed
end Decoder