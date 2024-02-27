package com.peknight.codec

import cats.data.Validated
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Contravariant, Foldable, Functor, Traverse}
import com.peknight.codec.instances.*
import com.peknight.codec.sum.{ArrayType, ObjectType, StringType}
import com.peknight.generic.priority.PriorityInstancesF2

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor

trait Encoder[F[_], S, A]:
  self =>
  def encode(a: A): F[S]
  def contramap[B](f: B => A): Encoder[F, S, B] = (b: B) => self.encode(f(b))
end Encoder
object Encoder extends EncoderStringInstances
  with EncoderArrayInstances
  with EncoderObjectInstances
  with EncoderNullInstances
  with EncoderMigrationInstances
  with EncoderDerivationInstances
  with PriorityInstancesF2[Encoder]:

  def apply[F[_], S, A](using encoder: Encoder[F, S, A]): Encoder[F, S, A] = encoder
  def instance[F[_], S, A](f: A => F[S]): Encoder[F, S, A] = f(_)
  given encoderContravariant[F[_], S, A]: Contravariant[[X] =>> Encoder[F, S, X]] with
    def contramap[A, B](fa: Encoder[F, S, A])(f: B => A): Encoder[F, S, B] = fa.contramap(f)
  end encoderContravariant

  def toStringEncoder[F[_]: Applicative, A]: Encoder[F, String, A] = (a: A) => a.toString.pure[F]

  def stringEncodeJavaTime[F[_]: Applicative, A <: TemporalAccessor](formatter: DateTimeFormatter)
  : Encoder[F, String, A] =
    (a: A) => formatter.format(a).pure[F]

  def traverseEncoder[F[_], S, A, G[_]](using traverse: Traverse[G], applicative: Applicative[F],
                                        encoder: Encoder[F, S, A]): Encoder[F, G[S], G[A]] =
    (a: G[A]) => a.traverse[F, S](encoder.encode)

  def vectorEncoder[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Vector[A]] =
    traverseEncoder[F, S, A, Vector]

  def vectorEncodeFoldable[F[_], S, A, G[_]](using applicative: Applicative[F], foldable: Foldable[G],
                                             encoder: Encoder[F, S, A]): Encoder[F, Vector[S], G[A]] =
    (a: G[A]) => a.foldLeft(Vector.empty[F[S]])((vector, v) => encoder.encode(v) +: vector).reverse.sequence

  def vectorEncodeIterable[F[_], S, A, G[X] <: Iterable[X]](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], G[A]] =
    vectorEncoder[F, S, A].contramap(_.toVector)

  def vectorEncodeIterableOnce[F[_], S, A, G[X] <: IterableOnce[X]](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], G[A]] =
    vectorEncoder[F, S, A].contramap(_.iterator.to(Vector))

  def objectEncodeEither[F[_], S, A, B](leftKey: String, rightKey: String)
                                       (using functor: Functor[F], encodeA: Encoder[F, S, A], encodeB: Encoder[F, S, B])
  : Encoder[F, Object[S], Either[A, B]] = {
    case Left(left) => encodeA.encode(left).map(l => Object.singleton(leftKey, l))
    case Right(right) => encodeB.encode(right).map(r => Object.singleton(rightKey, r))
  }

  def objectEncodeValidated[F[_], S, E, A](failureKey: String, successKey: String)
                                          (using functor: Functor[F], encodeE: Encoder[F, S, E],
                                           encodeA: Encoder[F, S, A]): Encoder[F, Object[S], Validated[E, A]] = {
    case Validated.Invalid(invalid) => encodeE.encode(invalid).map(l => Object.singleton(failureKey, l))
    case Validated.Valid(valid) => encodeA.encode(valid).map(r => Object.singleton(successKey, r))
  }

  def stringEncoder[F[_], S, A](encoder: Encoder[F, String, A])(using functor: Functor[F], stringType: StringType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(str => stringType.to(str))

  def arrayEncoder[F[_], S, A](encoder: Encoder[F, Vector[S], A])(using functor: Functor[F], arrayType: ArrayType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(arrayType.to)

  def objectEncoder[F[_], S, A](encoder: Encoder[F, Object[S], A])(using functor: Functor[F], objectType: ObjectType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(obj => objectType.to(objectType.fromObject(obj)))

end Encoder
