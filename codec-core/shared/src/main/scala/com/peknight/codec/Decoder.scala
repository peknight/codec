package com.peknight.codec

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, StateT, Validated, ValidatedNel}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Applicative, Apply, Eval, Functor, Monad, MonadError, SemigroupK}
import com.peknight.cats.ext.instances.applicative.given
import com.peknight.cats.ext.instances.eitherT.given
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.cursor.{Cursor, CursorType}
import com.peknight.codec.error.*
import com.peknight.codec.instances.*
import com.peknight.codec.sum.{ArrayType, ObjectType}
import com.peknight.generic.priority.PriorityInstancesF3

import java.time.format.DateTimeFormatter
import scala.collection.{Map, mutable}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

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
  with DecoderArrayInstances
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
  def cursor[F[_]: Applicative, S, A](f: SuccessCursor[S] => F[Either[DecodingFailure, A]])
  : Decoder[F, Cursor[S], DecodingFailure, A] =
    new Decoder[F, Cursor[S], DecodingFailure, A]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure, A]] =
        t match
          case cursor: SuccessCursor[S] => f(cursor)
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft[A].pure[F]
      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
        t match
          case cursor: SuccessCursor[S] => f(cursor).map(_.toValidatedNel)
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel[A].pure[F]
  end cursor
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

  given [F[_]: Monad, T, E]: SemigroupK[[X] =>> Decoder[F, T, E, X]] with MonadError[[X] =>> Decoder[F, T, E, X], E] with
    def combineK[A](x: Decoder[F, T, E, A], y: Decoder[F, T, E, A]): Decoder[F, T, E, A] = x.or(y)
    def pure[A](x: A): Decoder[F, T, E, A] = const(x)
    override def map[A, B](fa: Decoder[F, T, E, A])(f: A => B): Decoder[F, T, E, B] = fa.map(f)
    override def product[A, B](fa: Decoder[F, T, E, A], fb: Decoder[F, T, E, B]): Decoder[F, T, E, (A, B)] =
      fa.product(fb)
    override def ap[A, B](ff: Decoder[F, T, E, A => B])(fa: Decoder[F, T, E, A]): Decoder[F, T, E, B] =
      ff.product(fa).map {
        case (f, a) => f(a)
      }
    override def ap2[A, B, Z](ff: Decoder[F, T, E, (A, B) => Z])(fa: Decoder[F, T, E, A], fb: Decoder[F, T, E, B])
    : Decoder[F, T, E, Z] =
      ff.product(fa.product(fb)).map {
        case (f, (a, b)) => f(a, b)
      }
    override def map2[A, B, Z](fa: Decoder[F, T, E, A], fb: Decoder[F, T, E, B])(f: (A, B) => Z): Decoder[F, T, E, Z] =
      fa.product(fb).map {
        case (a, b) => f(a, b)
      }
    override def map2Eval[A, B, Z](fa: Decoder[F, T, E, A], fb: Eval[Decoder[F, T, E, B]])(f: (A, B) => Z)
    : Eval[Decoder[F, T, E, Z]] =
      fb.map(fb => map2(fa, fb)(f))
    override def productR[A, B](fa: Decoder[F, T, E, A])(fb: Decoder[F, T, E, B]): Decoder[F, T, E, B] =
      fa.product(fb).map(_._2)
    override def productL[A, B](fa: Decoder[F, T, E, A])(fb: Decoder[F, T, E, B]): Decoder[F, T, E, A] =
      fa.product(fb).map(_._1)
    def flatMap[A, B](fa: Decoder[F, T, E, A])(f: A => Decoder[F, T, E, B]): Decoder[F, T, E, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Decoder[F, T, E, Either[A, B]]): Decoder[F, T, E, B] =
      instance[F, T, E, B](t => Monad[[X] =>> F[Either[E, X]]].tailRecM[A, B](a)(a => f(a).decode(t)))
    def raiseError[A](e: E): Decoder[F, T, E, A] = Decoder.failed(e)
    def handleErrorWith[A](fa: Decoder[F, T, E, A])(f: E => Decoder[F, T, E, A]): Decoder[F, T, E, A] =
      fa.handleErrorWith(f)
  end given

  /**
   * Attempt to decode a value at key k and remove it from the Cursor[S]
   */
  def decodeField[F[_], S, A](k: String)(using Applicative[F], Decoder[F, Cursor[S], DecodingFailure, A], ObjectType[S])
  : StateT[[X] =>> F[Either[DecodingFailure, X]], Cursor[S], A] =
    StateT[[X] =>> F[Either[DecodingFailure, X]], Cursor[S], A] { c =>
      val field = c.downField(k)
      field.as[F, A].map {
        case Right(a) if field.failed => ((c, a)).asRight[DecodingFailure]
        case Right(a) => ((field.delete, a)).asRight[DecodingFailure]
        case left => left.asInstanceOf[Either[DecodingFailure, (Cursor[S], A)]]
      }
    }

  private[codec] def toBooleanOption(t: String): Option[Boolean] =
    if "true".equalsIgnoreCase(t) then Some(true)
    else if "false".equalsIgnoreCase(t) then Some(false)
    else if List("1", "t", "yes", "y").exists(t.equalsIgnoreCase) then Some(true)
    else if List("0", "f", "no", "n").exists(t.equalsIgnoreCase) then Some(false)
    else None

  private[codec] def decodeWithTry[F[_] : Applicative, A: ClassTag](f: String => A)
  : Decoder[F, String, DecodingFailure, A] =
    instance[F, String, DecodingFailure, A] { t =>
      Try(f(t)) match
        case Success(value) => value.asRight.pure
        case Failure(e) => ParsingTypeError[A](e).asLeft.pure
    }

  private[codec] def decodeNumber[F[_]: Applicative, A: ClassTag](f: BigDecimal => A)
  : Decoder[F, String, DecodingFailure, A] =
    decodeWithTry[F, A](t => f(BigDecimal(t)))

  private[codec] def decodeJavaTime[F[_]: Applicative, A: ClassTag](formatter: DateTimeFormatter)
                                                                   (f: (String, DateTimeFormatter) => A)
  : Decoder[F, String, DecodingFailure, A] =
    decodeWithTry(t => f(t, formatter))

  private[codec] def decodeMap[F[_], S, K, V, M[X, Y] <: Map[X, Y]](builder: => mutable.Builder[(K, V), M[K, V]])(
    using
    monad: Monad[F],
    keyDecoder: Decoder[F, String, DecodingFailure, K],
    valueDecoder: Decoder[F, Cursor[S], DecodingFailure, V],
    objectType: ObjectType[S]
  ): Decoder[F, Cursor[S], DecodingFailure, M[K, V]] =
    new Decoder[F, Cursor[S], DecodingFailure, M[K, V]]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure, M[K, V]]] =
        type EitherF[T] = F[Either[DecodingFailure, T]]
        t match
          case cursor: SuccessCursor[S] => objectType.asObject(cursor.value) match
            case Some(o) => objectType.keys(o).toList.foldLeftM[EitherF, mutable.Builder[(K, V), M[K, V]]](builder) {
              (builder, key) =>
                val c = t.downField(key)
                Monad[EitherF].flatMap(keyDecoder.decode(key).map(_.left.map(_.cursor(c)))) { k =>
                  Monad[EitherF].map(valueDecoder.decode(c))(v => builder += ((k, v)))
                }
            }.map(_.result())
            case _ => NotObject.cursor(t).asLeft.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]

      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure, M[K, V]]] =
        type ValidatedNelF[T] = F[ValidatedNel[DecodingFailure, T]]
        t match
          case cursor: SuccessCursor[S] => objectType.asObject(cursor.value) match
            case Some(o) => objectType.keys(o).toList.traverse[ValidatedNelF, (K, V)] {
              key =>
                val c = t.downField(key)
                (keyDecoder.decodeAccumulating(key), valueDecoder.decodeAccumulating(c)).mapN {
                  case (Valid(key), Valid(value)) => ((key, value)).validNel
                  case (Valid(_), Invalid(es)) => es.invalid
                  case (Invalid(es), Valid(_)) => es.map(_.cursor(c)).invalid
                  case (Invalid(keyEs), Invalid(valueEs)) =>
                    val keyErrors = keyEs.map(_.cursor(c))
                    NonEmptyList(keyErrors.head, keyErrors.tail ++ valueEs.toList).invalid
                }
            }.map(_.foldLeft(builder)(_ += _).result())
            case _ => NotObject.cursor(t).invalidNel.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel.pure[F]
  end decodeMap

  private[codec] def decodeSeq[F[_], S, A, C[_]](builder: => mutable.Builder[A, C[A]])(using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], DecodingFailure, A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], DecodingFailure, C[A]] =
    new Decoder[F, Cursor[S], DecodingFailure, C[A]]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure, C[A]]] =
        t match
          case cursor: SuccessCursor[S] => cursor.downArray match
            case arrayCursor: SuccessCursor[S] =>
              type EitherF[T] = F[Either[DecodingFailure, T]]
              Monad[EitherF].tailRecM[(Cursor[S], mutable.Builder[A, C[A]]), C[A]]((arrayCursor, builder)) {
                case (arrayCursor: SuccessCursor[S], builder) => decoder.decode(arrayCursor).map {
                  case Right(a) => ((arrayCursor.right, builder += a)).asLeft[C[A]].asRight[DecodingFailure]
                  case Left(e) => e.asLeft[Either[(Cursor[S], mutable.Builder[A, C[A]]), C[A]]]
                }
                case (arrayCursor: FailedCursor[S], builder) => builder.result().asRight.pure[EitherF]
              }
            case arrayCursor: FailedCursor[S] if arrayType.isArray(cursor.value) => builder.result().asRight.pure[F]
            case arrayCursor: FailedCursor[S] => NotArray.cursor(cursor).asLeft.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure, C[A]]] =
        t match
          case cursor: SuccessCursor[S] => cursor.downArray match
            case arrayCursor: SuccessCursor[S] =>
              type ValidatedNelF[T] = F[ValidatedNel[DecodingFailure, T]]
              List.unfold[SuccessCursor[S], Cursor[S]](arrayCursor) {
                case arrayCursor: SuccessCursor[S] => (arrayCursor, arrayCursor.right).some
                case arrayCursor: FailedCursor[S] => none[(SuccessCursor[S], Cursor[S])]
              }.traverse[ValidatedNelF, A](decoder.decodeAccumulating).map(_.foldLeft(builder)(_ += _).result())
            case arrayCursor: FailedCursor[S] if arrayType.isArray(cursor.value) => builder.result().validNel.pure[F]
            case arrayCursor: FailedCursor[S] => NotArray.cursor(cursor).invalidNel.pure[F]
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel.pure[F]
  end decodeSeq

  private[codec] def decodeNonEmptySeq[F[_], S, A, C[_], N](builder: => mutable.Builder[A, C[A]])
                                                           (create: (A, C[A]) => N)
                                                           (using monad: Monad[F],
                                                            decoder: Decoder[F, Cursor[S], DecodingFailure, A],
                                                            arrayType: ArrayType[S])
  : Decoder[F, Cursor[S], DecodingFailure, N] =
    new Decoder[F, Cursor[S], DecodingFailure, N]:
      def decode(t: Cursor[S]): F[Either[DecodingFailure, N]] =
        t match
          case cursor: SuccessCursor[S] =>
            val arrayCursor = cursor.downArray
            type EitherF[T] = F[Either[DecodingFailure, T]]
            Monad[EitherF].flatMap(decoder.decode(arrayCursor))(head =>
              Monad[EitherF].map(decodeSeq[F, S, A, C](builder).decode(arrayCursor.delete))(tail => create(head, tail))
            )
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
      def decodeAccumulating(t: Cursor[S]): F[ValidatedNel[DecodingFailure, N]] =
        t match
          case cursor: SuccessCursor[S] =>
            val arrayCursor = cursor.downArray
            type ValidatedNelF[T] = F[ValidatedNel[DecodingFailure, T]]
            Applicative[ValidatedNelF].map2(decoder.decodeAccumulating(arrayCursor),
              decodeSeq[F, S, A, C](builder).decodeAccumulating(arrayCursor.delete))(create)
          case cursor: FailedCursor[S] => cursor.toDecodingFailure.invalidNel.pure[F]
  end decodeNonEmptySeq

  private[codec] def decodeEither[F[_], S, A, B](leftKey: String, rightKey: String)(using
    applicative: Applicative[F],
    decodeA: Decoder[F, Cursor[S], DecodingFailure, A],
    decodeB: Decoder[F, Cursor[S], DecodingFailure, B],
    objectType: ObjectType[S],
  ): Decoder[F, Cursor[S], DecodingFailure, Either[A, B]] =
    cursor[F, S, Either[A, B]] { t => (t.downField(leftKey), t.downField(rightKey)) match
      case (lCursor: SuccessCursor[S], rCursor: SuccessCursor[S]) =>
        NotSingleKeyObject(List(leftKey, rightKey)).cursor(t).asLeft.pure
      case (lCursor: SuccessCursor[S], rCursor: FailedCursor[S]) => decodeA.decode(lCursor).map(_.map(_.asLeft))
      case (lCursor: FailedCursor[S], rCursor: SuccessCursor[S]) => decodeB.decode(rCursor).map(_.map(_.asRight))
      case (lCursor: FailedCursor[S], rCursor: FailedCursor[S]) => MissingField.cursor(t).asLeft.pure
    }

  private[codec] def decodeValidated[F[_], S, E, A](failureKey: String, successKey: String)(using
    applicative: Applicative[F],
    decodeE: Decoder[F, Cursor[S], DecodingFailure, E],
    decodeA: Decoder[F, Cursor[S], DecodingFailure, A],
    objectType: ObjectType[S],
  ): Decoder[F, Cursor[S], DecodingFailure, Validated[E, A]] =
    cursor[F, S, Validated[E, A]] { t => (t.downField(failureKey), t.downField(successKey)) match
        case (fCursor: SuccessCursor[S], sCursor: SuccessCursor[S]) =>
          NotSingleKeyObject(List(failureKey, successKey)).cursor(t).asLeft.pure
        case (fCursor: SuccessCursor[S], sCursor: FailedCursor[S]) => decodeE.decode(fCursor).map(_.map(_.invalid))
        case (fCursor: FailedCursor[S], sCursor: SuccessCursor[S]) => decodeA.decode(sCursor).map(_.map(_.valid))
        case (fCursor: FailedCursor[S], sCursor: FailedCursor[S]) => MissingField.cursor(t).asLeft.pure
    }

end Decoder