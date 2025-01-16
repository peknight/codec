package com.peknight.codec

import cats.data.Validated
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Contravariant, FlatMap, Foldable, Functor, Traverse}
import com.peknight.codec.derivation.EncoderDerivation
import com.peknight.codec.instances.*
import com.peknight.codec.number.Number
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.*
import com.peknight.generic.Generic
import com.peknight.generic.migration.Migration
import com.peknight.generic.priority.PriorityInstancesF2
import com.peknight.generic.tuple.Map

import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor

trait Encoder[F[_], S, A]:
  self =>
  def encode(a: A): F[S]
  def contramap[B](f: B => A): Encoder[F, S, B] = (b: B) => self.encode(f(b))
  def <<[SS](that: Encoder[F, SS, S])(using FlatMap[F]): Encoder[F, SS, A] =
    (a: A) => self.encode(a).flatMap(that.encode)
  def >>[B](that: Encoder[F, A, B])(using FlatMap[F]): Encoder[F, S, B] = that << self
end Encoder
object Encoder extends EncoderDerivation
  with EncoderValueInstances
  with EncoderArrayInstances
  with EncoderObjectInstances
  with EncoderNullInstances
  with EncoderIdentityInstances
  with PriorityInstancesF2[Encoder]:

  def apply[F[_], S, A](using encoder: Encoder[F, S, A]): Encoder[F, S, A] = encoder

  def instance[F[_], S, A](f: A => F[S]): Encoder[F, S, A] = f(_)

  def applicative[F[_]: Applicative, S, A](f: A => S): Encoder[F, S, A] = a => f(a).pure[F]

  def map[F[_]: Applicative, S, A](f: A => S): Encoder[F, S, A] = applicative[F, S, A](f)

  def identity[F[_]: Applicative, A]: Encoder[F, A, A] = map(Predef.identity)

  def const[F[_]: Applicative, S, A](s: S): Encoder[F, S, A] = applicative[F, S, A](_ => s)

  def forProduct[F[_], S, A, Repr <: Tuple](labels: Map[Repr, [X] =>> String])(f: A => Repr)(using
    applicative: Applicative[F], objectType: ObjectType[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], Repr]
  ): Encoder[F, S, A] =
    instance[F, S, A](a => handleForProduct[F, S, A, Repr](a)(labels)(f))

  private[codec] def handleForProduct[F[_], S, A, Repr <: Tuple](a: A)(labels: Map[Repr, [X] =>> String])(f: A => Repr)
                                                                (using
                                                                  applicative: Applicative[F],
                                                                  objectType: ObjectType[S],
                                                                  instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], Repr]
                                                                ): F[S] =
    instances.foldRightWithGivenLabel(f(a))(List.empty[F[(String, S)]])(labels.asInstanceOf) {
      [X] => (encoder: Encoder[F, S, X], x: X, label: String, acc: List[F[(String, S)]]) =>
        encoder.encode(x).map((label, _)) :: acc
    }.sequence.map(f => objectType.to(objectType.fromFoldable(f)))

  given encoderContravariant[F[_], S]: Contravariant[[X] =>> Encoder[F, S, X]] with
    def contramap[A, B](fa: Encoder[F, S, A])(f: B => A): Encoder[F, S, B] = fa.contramap(f)
  end encoderContravariant

  def encodeWithToString[F[_]: Applicative, A]: Encoder[F, String, A] = map[F, String, A](_.toString)

  def stringEncodeJavaTime[F[_] : Applicative, A <: TemporalAccessor](formatter: DateTimeFormatter)
  : Encoder[F, String, A] =
    map(formatter.format)

  def traverseEncoder[F[_], S, A, G[_]](
    using traverse: Traverse[G], applicative: Applicative[F], encoder: Encoder[F, S, A]
  ): Encoder[F, G[S], G[A]] =
    (a: G[A]) => a.traverse[F, S](encoder.encode)

  def vectorEncoder[F[_], S, A](using Applicative[F], Encoder[F, S, A]): Encoder[F, Vector[S], Vector[A]] =
    traverseEncoder[F, S, A, Vector]

  def vectorEncodeFoldable[F[_], S, A, G[_]](
    using applicative: Applicative[F], foldable: Foldable[G], encoder: Encoder[F, S, A]
  ): Encoder[F, Vector[S], G[A]] =
    (a: G[A]) => a.foldLeft(Vector.empty[F[S]])((vector, v) => encoder.encode(v) +: vector).reverse.sequence

  def vectorEncodeIterable[F[_], S, A, G[X] <: Iterable[X]](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], G[A]] =
    vectorEncoder[F, S, A].contramap(_.toVector)

  def vectorEncodeIterableOnce[F[_], S, A, G[X] <: IterableOnce[X]](using Applicative[F], Encoder[F, S, A])
  : Encoder[F, Vector[S], G[A]] =
    vectorEncoder[F, S, A].contramap(_.iterator.to(Vector))

  def objectEncodeEither[F[_], S, A, B](leftKey: String = "Left", rightKey: String = "Right")
                                       (using functor: Functor[F], encodeA: Encoder[F, S, A], encodeB: Encoder[F, S, B])
  : Encoder[F, Object[S], Either[A, B]] = {
    case Left(left) => encodeA.encode(left).map(l => Object.singleton(leftKey, l))
    case Right(right) => encodeB.encode(right).map(r => Object.singleton(rightKey, r))
  }

  def objectEncodeValidated[F[_], S, E, A](failureKey: String = "Invalid", successKey: String = "Valid")(
    using functor: Functor[F], encodeE: Encoder[F, S, E], encodeA: Encoder[F, S, A]
  ): Encoder[F, Object[S], Validated[E, A]] = {
    case Validated.Invalid(invalid) => encodeE.encode(invalid).map(l => Object.singleton(failureKey, l))
    case Validated.Valid(valid) => encodeA.encode(valid).map(r => Object.singleton(successKey, r))
  }
  
  def encodeA[F[_], S, A](using encoder: Encoder[F, Vector[S], A])(using functor: Functor[F], arrayType: ArrayType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(arrayType.to)

  def encodeB[F[_], S](using applicative: Applicative[F], booleanType: BooleanType[S]): Encoder[F, S, Boolean] =
    booleanType.to(_).pure[F]

  def encodeN[F[_], S, A](using encoder: Encoder[F, Number, A])(using functor: Functor[F], numberType: NumberType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(numberType.to)

  def encodeO[F[_], S, A](using encoder: Encoder[F, Object[S], A])(using functor: Functor[F], objectType: ObjectType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(obj => objectType.to(objectType.fromObject(obj)))

  def encodeS[F[_], S, A](using encoder: Encoder[F, String, A])(using functor: Functor[F], stringType: StringType[S])
  : Encoder[F, S, A] =
    encoder.encode(_).map(str => stringType.to(str))

  def stringEncodeWithNumberEncoder[F[_], A](using encoder: Encoder[F, Number, A])(using functor: Functor[F])
  : Encoder[F, String, A] =
    encoder.encode(_).map(_.toString)

  def encodeWithMigration[F[_], S, A](using migration: Migration[F, A, S]): Encoder[F, S, A] = migration.migrate(_)
end Encoder
