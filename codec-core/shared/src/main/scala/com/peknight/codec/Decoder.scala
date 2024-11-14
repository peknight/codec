package com.peknight.codec

import cats.data.Validated.{Invalid, Valid}
import cats.data.{StateT, Validated}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Applicative, Apply, Eval, Functor, Monad, MonadError, SemigroupK}
import com.peknight.cats.ext.instances.applicative.given
import com.peknight.cats.ext.instances.eitherT.given
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.cursor.Cursor.{FailedCursor, SuccessCursor}
import com.peknight.codec.error.*
import com.peknight.codec.instances.*
import com.peknight.codec.number.{BiggerDecimal, Number}
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.*
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.PriorityInstancesF2
import com.peknight.generic.{Generic, tuple}

import java.time.format.DateTimeFormatter
import scala.collection.{Map, mutable}
import scala.reflect.ClassTag
import scala.util.Try

trait Decoder[F[_], T, A]:
  self =>
  def decode(t: T): F[Either[DecodingFailure, A]]
  def map[B](f: A => B)(using Functor[F]): Decoder[F, T, B] =
    (t: T) => self.decode(t).map(_.map(f))
  end map
  def flatMap[B](f: A => Decoder[F, T, B])(using Monad[F]): Decoder[F, T, B] =
    (t: T) => self.decode(t).flatMap {
      case Right(a) => f(a).decode(t)
      case Left(e) => e.asLeft[B].pure[F]
    }
  end flatMap
  def handleErrorWith(f: DecodingFailure => Decoder[F, T, A])(using Monad[F]): Decoder[F, T, A] =
    (t: T) => self.decode(t).flatMap {
      case Left(e) => f(e).decode(t)
      case right => right.pure[F]
    }
  end handleErrorWith
  def ensure(pred: A => Boolean, error: T => DecodingFailure)(using Functor[F]): Decoder[F, T, A] =
    (t: T) => self.decode(t).map {
      case r@Right(a) => if pred(a) then r else error(t).asLeft[A]
      case left => left
    }
  end ensure
  def ensure(errors: A => List[DecodingFailure])(using Functor[F]): Decoder[F, T, A] =
    (t: T) => self.decode(t).map {
      case r@Right(a) => errors(a) match
        case Nil => r
        case head :: tail => DecodingFailure(head, tail).value(t).asLeft[A]
      case left => left
    }
  end ensure
  def validate(errors: T => List[DecodingFailure])(using Applicative[F]): Decoder[F, T, A] =
    (t: T) => errors(t) match
      case Nil => self.decode(t)
      case head :: tail => DecodingFailure(head, tail).asLeft[A].pure[F]
  end validate
  def validate[E](pred: T => Boolean, error: T => E)(using Applicative[F]): Decoder[F, T, A] =
    (t: T) => if pred(t) then self.decode(t) else DecodingFailure(error(t)).asLeft[A].pure[F]
  end validate
  def product[B](fb: Decoder[F, T, B])(using Apply[F]): Decoder[F, T, (A, B)] =
    (t: T) => (self.decode(t), fb.decode(t)).mapN { (ra, rb) =>
      (ra.toValidated, rb.toValidated).mapN((_, _)).toEither
    }
  end product
  def productWithMonad[B](fb: Decoder[F, T, B])(using Apply[F]): Decoder[F, T, (A, B)] =
    (t: T) => (self.decode(t), fb.decode(t)).mapN { (ra, rb) =>
      for
        a <- ra
        b <- rb
      yield (a, b)
    }
  end productWithMonad
  def or[AA >: A](d: => Decoder[F, T, AA])(using Monad[F]): Decoder[F, T, AA] =
    (t: T) => self.decode(t).flatMap {
      case Left(_) => d.decode(t)
      case right => right.pure[F]
    }
  end or
  def either[B](decodeB: Decoder[F, T, B])(using Monad[F]): Decoder[F, T, Either[A, B]] =
    (t: T) => self.decode(t).flatMap {
      case Right(a) => a.asLeft[B].asRight[DecodingFailure].pure[F]
      case _ => decodeB.decode(t).map {
        case Right(b) => b.asRight[A].asRight[DecodingFailure]
        case left => left.asInstanceOf[Either[DecodingFailure, Either[A, B]]]
      }
    }
  end either
  def prepare[TT](f: TT => T): Decoder[F, TT, A] =
    (t: TT) => self.decode(f(t))
  end prepare
  def emap[B](f: A => T => Either[DecodingFailure, B])(using Functor[F]): Decoder[F, T, B] =
    (t: T) => self.decode(t).map {
      case Right(a) => f(a)(t)
      case Left(e) => e.asLeft[B]
    }
  end emap
  def >>[TT](that: Decoder[F, TT, T])(using Monad[F]): Decoder[F, TT, A] =
    (tt: TT) => that.decode(tt).flatMap {
      case Right(t) => self.decode(t)
      case Left(e) => e.asLeft[A].pure[F]
    }
  def <<[B](that: Decoder[F, A, B])(using Monad[F]): Decoder[F, T, B] = that >> self
end Decoder
object Decoder extends DecoderCursorInstances
  with DecoderValueInstances
  with DecoderArrayInstances
  with DecoderObjectInstances
  with DecoderNullInstances
  with DecoderDerivationInstances
  with DecoderIdentityInstances
  with PriorityInstancesF2[Decoder]:

  def apply[F[_], T, A](using decoder: Decoder[F, T, A]): Decoder[F, T, A] = decoder

  def instance[F[_], T, A](f: T => F[Either[DecodingFailure, A]]): Decoder[F, T, A] = f(_)

  def applicative[F[_]: Applicative, T, A](f: T => Either[DecodingFailure, A]): Decoder[F, T, A] =
    instance[F, T, A](t => f(t).pure[F])

  def map[F[_]: Applicative, T, A](f: T => A): Decoder[F, T, A] =
    applicative[F, T, A](t => f(t).asRight[DecodingFailure])

  def mapOption[F[_]: Applicative, T, A: ClassTag](f: T => Option[A]): Decoder[F, T, A] =
    applicative[F, T, A](t => f(t).toRight(WrongClassTag[A].value(t)))

  def mapTry[F[_]: Applicative, T, A: ClassTag](f: T => Try[A]): Decoder[F, T, A] =
    applicative[F, T, A](t => f(t).toEither.left.map(e => ParsingTypeError[A](e).value(t)))

  def parse[F[_]: Applicative, T, A: ClassTag](f: T => A): Decoder[F, T, A] =
    mapTry[F, T, A](t => Try(f(t)))

  def identity[F[_]: Applicative, A]: Decoder[F, A, A] = map[F, A, A](Predef.identity)

  def const[F[_]: Applicative, T, A](a: A): Decoder[F, T, A] = map(_ => a)

  def failed[F[_] : Applicative, T, A](failure: DecodingFailure): Decoder[F, T, A] = _ => failure.asLeft[A].pure[F]

  def failed[F[_] : Applicative, T, A](f: T => DecodingFailure): Decoder[F, T, A] = (t: T) => f(t).asLeft[A].pure[F]

  def cursor[F[_]: Applicative, S, A](f: SuccessCursor[S] => F[Either[DecodingFailure, A]])
  : Decoder[F, Cursor[S], A] =
    case cursor: SuccessCursor[S] => f(cursor)
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft[A].pure[F]
  end cursor
  def cursorApplicative[F[_]: Applicative, S, A](f: SuccessCursor[S] => Either[DecodingFailure, A])
  : Decoder[F, Cursor[S], A] =
    cursor[F, S, A](t => f(t).pure[F])

  def cursorMap[F[_]: Applicative, S, A](f: SuccessCursor[S] => A): Decoder[F, Cursor[S], A] =
    cursorApplicative[F, S, A](t => f(t).asRight[DecodingFailure])

  def cursorIdentity[F[_]: Applicative, S]: Decoder[F, Cursor[S], S] = cursorMap[F, S, S](_.value)

  def cursorConst[F[_]: Applicative, S, A](a: A): Decoder[F, Cursor[S], A] = cursorMap[F, S, A](_ => a)

  def cursorValue[F[_]: Applicative, S, A](f: S => F[Either[DecodingFailure, A]]): Decoder[F, Cursor[S], A] =
    cursor[F, S, A](t => f(t.value).map(_.left.map(_.cursor(t))))

  def cursorValueApplicative[F[_]: Applicative, S, A](f: S => Either[DecodingFailure, A]): Decoder[F, Cursor[S], A] =
    cursorApplicative[F, S, A](t => f(t.value).left.map(_.cursor(t)))

  def cursorValueMap[F[_]: Applicative, S, A](f: S => A): Decoder[F, Cursor[S], A] = cursorMap[F, S, A](t => f(t.value))

  def fromState[F[_]: Monad, T, A](s: StateT[[X] =>> F[Either[DecodingFailure, X]], T, A]): Decoder[F, T, A] = s.runA(_)

  def forProduct[F[_], S, A, Repr <: Tuple](labels: tuple.Map[Repr, [X] =>> String])(f: Repr => Either[DecodingFailure, A])(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], Repr]
  ): Decoder[F, Cursor[S], A] =
    cursor[F, S, A](t => handleForProduct[F, S, A, Repr](t)(labels)(f))

  private[codec] def handleForProduct[F[_], S, A, Repr <: Tuple](cursor: SuccessCursor[S])
                                                                (labels: tuple.Map[Repr, [X] =>> String])
                                                                (f: Repr => Either[DecodingFailure, A])
                                                                (
                                                                  using
                                                                  applicative: Applicative[F],
                                                                  objectType: ObjectType[S],
                                                                  nullType: NullType[S],
                                                                  instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], Repr]
                                                                ): F[Either[DecodingFailure, A]] =
    instances.constructWithGivenLabel[[X] =>> F[Validated[DecodingFailure, X]]](labels.asInstanceOf) {
      [X] => (decoder: Decoder[F, Cursor[S], X], label: String) =>
        decoder.decode(cursor.downField(label)).map(_.toValidated)
    }.map(_.toEither.flatMap(f))

  def forProductMap[F[_], S, A, Repr <: Tuple](labels: tuple.Map[Repr, [X] =>> String])(f: Repr => A)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], Repr]
  ): Decoder[F, Cursor[S], A] =
    forProduct[F, S, A, Repr](labels)(repr => f(repr).asRight)

  given [F[_]: Monad, T]: SemigroupK[[X] =>> Decoder[F, T, X]]
    with MonadError[[X] =>> Decoder[F, T, X], DecodingFailure] with
    def combineK[A](x: Decoder[F, T, A], y: Decoder[F, T, A]): Decoder[F, T, A] = x.or(y)
    def pure[A](x: A): Decoder[F, T, A] = const(x)
    override def map[A, B](fa: Decoder[F, T, A])(f: A => B): Decoder[F, T, B] = fa.map(f)
    override def product[A, B](fa: Decoder[F, T, A], fb: Decoder[F, T, B]): Decoder[F, T, (A, B)] = fa.product(fb)
    override def ap[A, B](ff: Decoder[F, T, A => B])(fa: Decoder[F, T, A]): Decoder[F, T, B] =
      ff.product(fa).map {
        case (f, a) => f(a)
      }
    override def ap2[A, B, Z](ff: Decoder[F, T, (A, B) => Z])(fa: Decoder[F, T, A], fb: Decoder[F, T, B])
    : Decoder[F, T, Z] =
      ff.product(fa.product(fb)).map {
        case (f, (a, b)) => f(a, b)
      }
    override def map2[A, B, Z](fa: Decoder[F, T, A], fb: Decoder[F, T, B])(f: (A, B) => Z): Decoder[F, T, Z] =
      fa.product(fb).map {
        case (a, b) => f(a, b)
      }
    override def map2Eval[A, B, Z](fa: Decoder[F, T, A], fb: Eval[Decoder[F, T, B]])(f: (A, B) => Z)
    : Eval[Decoder[F, T, Z]] =
      fb.map(fb => map2(fa, fb)(f))
    override def productR[A, B](fa: Decoder[F, T, A])(fb: Decoder[F, T, B]): Decoder[F, T, B] =
      fa.product(fb).map(_._2)
    override def productL[A, B](fa: Decoder[F, T, A])(fb: Decoder[F, T, B]): Decoder[F, T, A] =
      fa.product(fb).map(_._1)
    def flatMap[A, B](fa: Decoder[F, T, A])(f: A => Decoder[F, T, B]): Decoder[F, T, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Decoder[F, T, Either[A, B]]): Decoder[F, T, B] =
      (t: T) => Monad[[X] =>> F[Either[DecodingFailure, X]]].tailRecM[A, B](a)(a => f(a).decode(t))
    def raiseError[A](e: DecodingFailure): Decoder[F, T, A] = Decoder.failed(e)
    def handleErrorWith[A](fa: Decoder[F, T, A])(f: DecodingFailure => Decoder[F, T, A]): Decoder[F, T, A] =
      fa.handleErrorWith(f)
  end given

  extension [F[_], S, E, A] (decoder: Decoder[F, Cursor[S], A])
    def decodeS(s: S): F[Either[DecodingFailure, A]] = decoder.decode(Cursor.from(s))
    def at(field: String)(using ObjectType[S]): Decoder[F, Cursor[S], A] =
      (cursor: Cursor[S]) => decoder.decode(cursor.downField(field))
    end at
  end extension

  /**
   * Attempt to decode a value at key k and remove it from the Cursor[S]
   */
  def decodeField[F[_], S, A](k: String)(using Applicative[F], Decoder[F, Cursor[S], A], ObjectType[S])
  : StateT[[X] =>> F[Either[DecodingFailure, X]], Cursor[S], A] =
    StateT[[X] =>> F[Either[DecodingFailure, X]], Cursor[S], A] { c =>
      val field = c.downField(k)
      field.as[F, A].map {
        case Right(a) if field.failed => ((c, a)).asRight[DecodingFailure]
        case Right(a) => ((field.delete, a)).asRight[DecodingFailure]
        case left => left.asInstanceOf[Either[DecodingFailure, (Cursor[S], A)]]
      }
    }

  def toBooleanOption(t: String): Option[Boolean] =
    if "true".equalsIgnoreCase(t) then Some(true)
    else if "false".equalsIgnoreCase(t) then Some(false)
    else if List("1", "t", "yes", "y", "on").exists(t.equalsIgnoreCase) then Some(true)
    else if List("0", "f", "no", "n", "off").exists(t.equalsIgnoreCase) then Some(false)
    else None

  def parseNumber(input: String): Either[DecodingFailure, Number] =
    BiggerDecimal.parseBiggerDecimal(input)
      .left.map(DecodingFailure.apply)
      .flatMap(_.toRight(NotNumber.value(input)))
      .map(biggerDecimal => Number.fromBiggerDecimal(biggerDecimal))

  def objectUnit[S](s: S)(using objectType: ObjectType[S]): Option[Unit] =
    objectType.asObject(s).filter(objectType.isEmpty).as(())

  def arrayUnit[S](s: S)(using arrayType: ArrayType[S]): Option[Unit] =
    arrayType.asArray(s).filter(_.isEmpty).as(())

  def nullUnit[S](s: S)(using nullType: NullType[S]): Option[Unit] = nullType.asNull(s)

  def decodeUnit[F[_] : Applicative, S](t: SuccessCursor[S])(f: S => Option[Unit]): F[Either[DecodingFailure, Unit]] =
    f(t.value) match
      case Some(_) => ().asRight.pure
      case None => NotUnit.cursor(t).asLeft.pure
  end decodeUnit

  def numberDecodeNumber[F[_]: Applicative, A](f: Number => A): Decoder[F, Number, A] = map[F, Number, A](f)

  def numberDecodeNumberOption[F[_]: Applicative, A: ClassTag](f: Number => Option[A]): Decoder[F, Number, A] =
    applicative[F, Number, A](t => f(t).toRight(WrongClassTag[A].value(t)))

  def stringDecodeWithNumber[F[_] : Applicative, A: ClassTag](f: Number => A): Decoder[F, String, A] =
    stringDecodeWithNumberOption[F, A](number => Some(f(number)))

  def stringDecodeWithNumberOption[F[_] : Applicative, A: ClassTag](f: Number => Option[A]): Decoder[F, String, A] =
    applicative[F, String, A](t => parseNumber(t).flatMap(number => f(number).toRight(WrongClassTag[A].value(t))))

  def stringDecodeJavaTime[F[_]: Applicative, A: ClassTag](formatter: DateTimeFormatter)
                                                          (f: (String, DateTimeFormatter) => A): Decoder[F, String, A] =
    parse(t => f(t, formatter))

  def handleDecodeSeq[F[_], S, A, C[_]](builder: => mutable.Builder[A, C[A]])
                                       (f: List[SuccessCursor[S]] => F[Either[DecodingFailure, C[A]]])
                                       (using applicative: Applicative[F], arrayType: ArrayType[S])
  : Decoder[F, Cursor[S], C[A]] =
    case cursor: SuccessCursor[S] => cursor.downArray match
      case arrayCursor: SuccessCursor[S] =>
        val cursors = List.unfold[SuccessCursor[S], Cursor[S]](arrayCursor) {
          case arrayCursor: SuccessCursor[S] => (arrayCursor, arrayCursor.right).some
          case arrayCursor: FailedCursor[S] => none[(SuccessCursor[S], Cursor[S])]
        }
        f(cursors)
      case arrayCursor: FailedCursor[S] if arrayType.isArray(cursor.value) => builder.result().asRight.pure[F]
      case arrayCursor: FailedCursor[S] => NotArray.cursor(cursor).asLeft.pure[F]
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end handleDecodeSeq

  def decodeSeq[F[_], S, A, C[_]](builder: => mutable.Builder[A, C[A]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], C[A]] =
    type ValidatedF[T] = F[Validated[DecodingFailure, T]]
    handleDecodeSeq[F, S, A, C](builder) { cursors =>
      val res: F[Validated[DecodingFailure, C[A]]] = cursors
        .traverse[ValidatedF, A](tt => decoder.decode(tt).map(_.toValidated))
        .map(_.foldLeft(builder)(_ += _).result())
      res.map(_.toEither)
    }
  end decodeSeq

  def decodeSeqIgnoreError[F[_], S, A, C[_]](builder: => mutable.Builder[A, C[A]])(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], C[A]] =
    handleDecodeSeq[F, S, A, C](builder)(
      _.traverse[F, Either[DecodingFailure, A]](tt => decoder.decode(tt))
        .map(_.foldLeft(builder)((builder, either) => either.fold(_ => builder, a => builder += a)).result().asRight)
    )
  end decodeSeqIgnoreError

  def decodeSeqWithMonad[F[_], S, A, C[_]](builder: => mutable.Builder[A, C[A]])(
    using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], C[A]] =
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
  end decodeSeqWithMonad
  def decodeNonEmptySeq[F[_], S, A, C[_], N](builder: => mutable.Builder[A, C[A]])(create: (A, C[A]) => N)(
    using
    applicative: Applicative[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], N] =
    case cursor: SuccessCursor[S] =>
      val arrayCursor = cursor.downArray
      type ValidatedF[T] = F[Validated[DecodingFailure, T]]
      val res: F[Validated[DecodingFailure, N]] = Applicative[ValidatedF].map2(
        decoder.decode(arrayCursor).map(_.toValidated),
        decodeSeq[F, S, A, C](builder).decode(arrayCursor.delete).map(_.toValidated)
      )(create)
      res.map(_.toEither)
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end decodeNonEmptySeq
  def decodeNonEmptySeqWithMonad[F[_], S, A, C[_], N](builder: => mutable.Builder[A, C[A]])(create: (A, C[A]) => N)(
    using
    monad: Monad[F],
    decoder: Decoder[F, Cursor[S], A],
    arrayType: ArrayType[S]
  ): Decoder[F, Cursor[S], N] =
    case cursor: SuccessCursor[S] =>
      val arrayCursor = cursor.downArray
      type EitherF[T] = F[Either[DecodingFailure, T]]
      Monad[EitherF].flatMap(decoder.decode(arrayCursor))(head =>
        Monad[EitherF].map(decodeSeqWithMonad[F, S, A, C](builder).decode(arrayCursor.delete))(
          tail => create(head, tail)
        )
      )
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end decodeNonEmptySeqWithMonad

  def decodeMap[F[_], S, K, V, M[X, Y] <: Map[X, Y]](builder: => mutable.Builder[(K, V), M[K, V]])(
    using
    applicative: Applicative[F],
    keyDecoder: Decoder[F, String, K],
    valueDecoder: Decoder[F, Cursor[S], V],
    objectType: ObjectType[S]
  ): Decoder[F, Cursor[S], M[K, V]] =
    case cursor: SuccessCursor[S] => objectType.asObject(cursor.value) match
      case Some(o) =>
        type ValidatedF[T] = F[Validated[DecodingFailure, T]]
        val res: F[Validated[DecodingFailure, M[K, V]]] =
          objectType.keys(o).toList.traverse[ValidatedF, (K, V)] { key =>
            val c = cursor.downField(key)
            (keyDecoder.decode(key).map(_.toValidated), valueDecoder.decode(c).map(_.toValidated)).mapN {
              case (Valid(key), Valid(value)) => ((key, value)).valid
              case (Valid(_), Invalid(es)) => es.invalid
              case (Invalid(es), Valid(_)) => es.cursor(c).invalid
              case (Invalid(keyEs), Invalid(valueEs)) => (keyEs.cursor(c) |+| valueEs).invalid
            }
          }.map(_.foldLeft(builder)(_ += _).result())
        res.map(_.toEither)
      case None => NotObject.cursor(cursor).asLeft.pure[F]
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end decodeMap

  def decodeMapWithMonad[F[_], S, K, V, M[X, Y] <: Map[X, Y]](builder: => mutable.Builder[(K, V), M[K, V]])(
    using
    monad: Monad[F],
    keyDecoder: Decoder[F, String, K],
    valueDecoder: Decoder[F, Cursor[S], V],
    objectType: ObjectType[S]
  ): Decoder[F, Cursor[S], M[K, V]] =
    case cursor: SuccessCursor[S] => objectType.asObject(cursor.value) match
      case Some(o) =>
        type EitherF[T] = F[Either[DecodingFailure, T]]
        objectType.keys(o).toList.foldLeftM[EitherF, mutable.Builder[(K, V), M[K, V]]](builder) {
          (builder, key) =>
            val c = cursor.downField(key)
            Monad[EitherF].flatMap(keyDecoder.decode(key).map(_.left.map(_.cursor(c)))) { k =>
              Monad[EitherF].map(valueDecoder.decode(c))(v => builder += ((k, v)))
            }
        }.map(_.result())
      case _ => NotObject.cursor(cursor).asLeft.pure[F]
    case cursor: FailedCursor[S] => cursor.toDecodingFailure.asLeft.pure[F]
  end decodeMapWithMonad

  def decodeEither[F[_], S, A, B](leftKey: String = "Left", rightKey: String = "Right")(
    using
    applicative: Applicative[F],
    decodeA: Decoder[F, Cursor[S], A],
    decodeB: Decoder[F, Cursor[S], B],
    objectType: ObjectType[S],
  ): Decoder[F, Cursor[S], Either[A, B]] =
    cursor[F, S, Either[A, B]] { t => (t.downField(leftKey), t.downField(rightKey)) match
      case (lCursor: SuccessCursor[S], rCursor: SuccessCursor[S]) =>
        NotSingleKeyObject(List(leftKey, rightKey)).cursor(t).asLeft.pure
      case (lCursor: SuccessCursor[S], rCursor: FailedCursor[S]) => decodeA.decode(lCursor).map(_.map(_.asLeft))
      case (lCursor: FailedCursor[S], rCursor: SuccessCursor[S]) => decodeB.decode(rCursor).map(_.map(_.asRight))
      case (lCursor: FailedCursor[S], rCursor: FailedCursor[S]) => MissingField.cursor(t).asLeft.pure
    }

  def decodeValidated[F[_], S, E, A](failureKey: String = "Invalid", successKey: String = "Valid")(
    using
    applicative: Applicative[F],
    decodeE: Decoder[F, Cursor[S], E],
    decodeA: Decoder[F, Cursor[S], A],
    objectType: ObjectType[S],
  ): Decoder[F, Cursor[S], Validated[E, A]] =
    cursor[F, S, Validated[E, A]] { t => (t.downField(failureKey), t.downField(successKey)) match
        case (fCursor: SuccessCursor[S], sCursor: SuccessCursor[S]) =>
          NotSingleKeyObject(List(failureKey, successKey)).cursor(t).asLeft.pure
        case (fCursor: SuccessCursor[S], sCursor: FailedCursor[S]) => decodeE.decode(fCursor).map(_.map(_.invalid))
        case (fCursor: FailedCursor[S], sCursor: SuccessCursor[S]) => decodeA.decode(sCursor).map(_.map(_.valid))
        case (fCursor: FailedCursor[S], sCursor: FailedCursor[S]) => MissingField.cursor(t).asLeft.pure
    }

  def decodeB[F[_], S](using applicative: Applicative[F], booleanType: BooleanType[S])
  : Decoder[F, Cursor[S], Boolean] =
    cursorValueApplicative[F, S, Boolean] { t =>
      booleanType.asBoolean(t) match
        case Some(b) => b.asRight
        case None => WrongClassTag[Boolean].asLeft
    }

  def decodeBS[F[_], S](using applicative: Applicative[F], booleanType: BooleanType[S], stringType: StringType[S])
  : Decoder[F, Cursor[S], Boolean] =
    cursorValue[F, S, Boolean] { t =>
      booleanType.asBoolean(t) match
        case Some(b) => b.asRight.pure[F]
        case None => stringType.asString(t) match
          case Some(s) => mapOption(toBooleanOption).decode(s)
          case None => WrongClassTag[Boolean].asLeft.pure[F]
    }

  def decodeN[F[_], S, A](using decoder: Decoder[F, Number, A])(
    using
    applicative: Applicative[F],
    numberType: NumberType[S],
    classTag: ClassTag[A]
  ): Decoder[F, Cursor[S], A] =
    cursorValue[F, S, A] { t =>
      numberType.asNumber(t) match
        case Some(n) => decoder.decode(n)
        case None => WrongClassTag[A].asLeft.pure
    }

  def decodeNS[F[_], S, A](using decoder: Decoder[F, Number, A])(
    using
    applicative: Applicative[F],
    numberType: NumberType[S],
    stringType: StringType[S],
    classTag: ClassTag[A]
  ): Decoder[F, Cursor[S], A] =
    cursorValue[F, S, A] { t =>
      numberType.asNumber(t) match
        case Some(n) => decoder.decode(n)
        case None => stringType.asString(t) match
          case Some(s) => stringDecodeWithNumberDecoder[F, A].decode(s)
          case None => WrongClassTag[A].asLeft.pure
    }

  def decodeO[F[_], S, A](using decoder: Decoder[F, Object[S], A])(
    using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, Object[S]]
  ): Decoder[F, Cursor[S], A] =
    cursorValue[F, S, A] { t =>
      objectType.asObject(t) match
        case Some(o) => decoder.decode(o)
        case None => NotObject.asLeft.pure
    }

  def decodeS[F[_], S, A](using decoder: Decoder[F, String, A])(
    using
    applicative: Applicative[F],
    stringType: StringType[S],
    classTag: ClassTag[A]
  ): Decoder[F, Cursor[S], A] =
    cursor[F, S, A] { t =>
      StringType[S].asString(t.value) match
        case Some(s) => decoder.decode(s).map(_.left.map(_.cursor(t)))
        case None => WrongClassTag[A].cursor(t).asLeft.pure
    }

  def stringDecodeWithNumberDecoder[F[_], A](using decoder: Decoder[F, Number, A])(using applicative: Applicative[F])
  : Decoder[F, String, A] =
    (t: String) => parseNumber(t) match
      case Right(n) => decoder.decode(n)
      case Left(error) => error.asLeft.pure

  def decodeWithMigration[F[_], T, A](using migration: Migration[F, T, A])(using functor: Functor[F]): Decoder[F, T, A] =
    (t: T) => migration.migrate(t).map(_.asRight[DecodingFailure])

  def decodeWithEncoder[F[_], T, A](using encoder: Encoder[F, A, T])(using functor: Functor[F]): Decoder[F, T, A] =
    (t: T) => encoder.encode(t).map(_.asRight[DecodingFailure])
end Decoder