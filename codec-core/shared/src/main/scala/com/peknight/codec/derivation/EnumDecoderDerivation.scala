package com.peknight.codec.derivation

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import cats.{Applicative, Functor}
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.Configuration
import com.peknight.codec.error.{DecodingFailure, NoSuchEnum}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons
import com.peknight.generic.migration.id.Migration

trait EnumDecoderDerivation:
  inline def derived[F[_], T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, DecodingFailure, String],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, DecodingFailure, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new EnumDecoder[F, T, DecodingFailure, A]:
      def decoders: Map[String, Decoder[F, T, DecodingFailure, _]] =
        enumDecodersDict[F, T, DecodingFailure, A](this, configuration, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        decodeEnumEither[F, T, A, generic.Repr](t, configuration, stringDecoder, generic, singletons)
      def decodeAccumulating(t: T): F[ValidatedNel[DecodingFailure, A]] =
        decodeEnumValidatedNel[F, T, A, generic.Repr](t, configuration, stringDecoder, generic, singletons)
  end derived

  private[derivation] def decodeEnumEither[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, DecodingFailure, String],
    generic: Generic.Sum[A],
    singletons: Repr
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) =>
        handleDecodeEnum[[X] =>> Either[DecodingFailure, X], T, A, Repr](t, caseName, configuration, _.asLeft[A], generic,
          singletons)
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def decodeEnumValidatedNel[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, DecodingFailure, String],
    generic: Generic.Sum[A],
    singletons: Repr
  ): F[ValidatedNel[DecodingFailure, A]] =
    stringDecoder.decodeAccumulating(t).map {
      case Valid(caseName) =>
        handleDecodeEnum[[X] =>> ValidatedNel[DecodingFailure, X], T, A, Repr](t, caseName, configuration,
          _.invalidNel[A], generic, singletons)
      case Invalid(e) => e.invalid[A]
    }

  private[this] def handleDecodeEnum[G[_]: Applicative, T, A, Repr <: Tuple](
    t: T,
    caseName: String,
    configuration: Configuration,
    asLeft: DecodingFailure => G[A],
    generic: Generic.Sum[A],
    singletons: Repr
  ): G[A] =
    generic.labels.zip(singletons).toList.asInstanceOf[List[(String, A)]]
      .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
      .map(_._2)
      .fold(asLeft(NoSuchEnum(caseName).label(generic.label).value(t)))(_.pure[G])

  private[derivation] def enumDecodersDict[F[_], T, E, A](
    decoder: Decoder[F, T, E, A],
    configuration: Configuration,
    generic: Generic.Sum[A]
  ): Map[String, Decoder[F, T, E, A]] =
    generic.labels.toList.asInstanceOf[List[String]]
      .map(label => (configuration.transformConstructorNames(label), decoder))
      .toMap

end EnumDecoderDerivation
object EnumDecoderDerivation extends EnumDecoderDerivation