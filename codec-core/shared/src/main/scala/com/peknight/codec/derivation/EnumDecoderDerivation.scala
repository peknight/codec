package com.peknight.codec.derivation

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import cats.{Applicative, Functor}
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.error.{DecodingFailure, NoSuchEnum}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons
import com.peknight.generic.migration.id.Migration

trait EnumDecoderDerivation:
  inline def derived[F[_], T, E, A](using configuration: DecoderConfiguration)(using
    functor: Functor[F],
    failure: Migration[DecodingFailure[T], E],
    stringDecoder: Decoder[F, T, E, String],
    generic0: Generic.Sum[A]
  ): EnumDecoder[F, T, E, A] =
    val singletons = summonAllSingletons[generic0.Repr](generic0.label)
    new EnumDecoder[F, T, E, A]:
      def generic: Generic.Sum[A] = generic0
      def decode(t: T): F[Either[E, A]] =
        decodeEnumEither[F, T, E, A, generic0.Repr](t, configuration, failure, stringDecoder, generic, singletons)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        decodeEnumValidatedNel[F, T, E, A, generic0.Repr](t, configuration, failure, stringDecoder, generic, singletons)
  end derived

  private[derivation] def decodeEnumEither[F[_]: Functor, T, E, A, Repr <: Tuple](
    t: T,
    configuration: DecoderConfiguration,
    failure: Migration[DecodingFailure[T], E],
    stringDecoder: Decoder[F, T, E, String],
    generic: Generic.Sum[A],
    singletons: Repr
  ): F[Either[E, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) =>
        handleDecodeEnum[[X] =>> Either[E, X], T, E, A, Repr](t, caseName, configuration, failure, _.asLeft[A], generic,
          singletons)
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def decodeEnumValidatedNel[F[_]: Functor, T, E, A, Repr <: Tuple](
    t: T,
    configuration: DecoderConfiguration,
    failure: Migration[DecodingFailure[T], E],
    stringDecoder: Decoder[F, T, E, String],
    generic: Generic.Sum[A],
    singletons: Repr
  ): F[ValidatedNel[E, A]] =
    stringDecoder.decodeAccumulating(t).map {
      case Valid(caseName) =>
        handleDecodeEnum[[X] =>> ValidatedNel[E, X], T, E, A, Repr](t, caseName, configuration, failure,
          _.invalidNel[A], generic, singletons)
      case Invalid(e) => e.invalid[A]
    }

  private[this] def handleDecodeEnum[G[_]: Applicative, T, E, A, Repr <: Tuple](
    t: T,
    caseName: String,
    configuration: DecoderConfiguration,
    failure: Migration[DecodingFailure[T], E],
    asLeft: E => G[A],
    generic: Generic.Sum[A],
    singletons: Repr
  ): G[A] =
    generic.labels.zip(singletons).toList.asInstanceOf[List[(String, A)]]
      .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
      .map(_._2)
      .fold(asLeft(failure.migrate(NoSuchEnum(t, generic.label, caseName))))(_.pure[G])
end EnumDecoderDerivation
object EnumDecoderDerivation extends EnumDecoderDerivation