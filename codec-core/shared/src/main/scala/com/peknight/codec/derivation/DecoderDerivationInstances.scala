package com.peknight.codec.derivation

import cats.data.ValidatedNel
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.validated.*
import cats.{Applicative, Monad}
import com.peknight.cats.ext.instances.applicative.given
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.DecoderConfiguration
import com.peknight.codec.error.*
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration

trait DecoderDerivationInstances:
  def derived[F[_], T, E, A](using configuration: DecoderConfiguration)(using
    monad: Monad[F],
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Decoder[F, T, E, A] =
    instances.derive(
      inst ?=> derivedProduct[F, T, E, A](configuration, decodeObjectOps, failure, inst),
      inst ?=> derivedSum[F, T, E, A](configuration, decodeObjectOps, failure, stringOptionDecoder, inst)
    )

  private[this] def derivedProduct[F[_] : Applicative, T, E, A](
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Decoder[F, T, E, A] =
    new Decoder[F, T, E, A]:
      def decode(t: T): F[Either[E, A]] = decodeProductEither(t, configuration, decodeObjectOps, failure, instances)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        decodeProductValidatedNel(t, configuration, decodeObjectOps, failure, instances)
  end derivedProduct

  private[this] def derivedSum[F[_] : Monad, T, E, A](
    configuration0: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): SumDecoder[F, T, E, A] =
    new SumDecoder[F, T, E, A]:
      def configuration: DecoderConfiguration = configuration0
      def decoders: Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A] = instances
      def decode(t: T): F[Either[E, A]] =
        decodeSumEither(t, configuration0, decodeObjectOps, failure, stringOptionDecoder, instances)
      def decodeAccumulating(t: T): F[ValidatedNel[E, A]] =
        decodeSumValidatedNel(t, configuration0, decodeObjectOps, failure, stringOptionDecoder, instances)
  end derivedSum

  private[derivation] def decodeProductEither[F[_] : Applicative, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[Either[E, A]] =
    decodeProduct[F, [X] =>> Either[E, X], T, E, A](
      t,
      configuration,
      decodeObjectOps,
      failure,
      _.asLeft[A],
      [X] => (result: Either[E, X]) => result.isRight,
      [X] => (decoder: Decoder[F, T, E, X]) => decoder.decode,
      instances
    )

  private[derivation] def decodeProductValidatedNel[F[_] : Applicative, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[ValidatedNel[E, A]] =
    decodeProduct[F, [X] =>> ValidatedNel[E, X], T, E, A](
      t,
      configuration,
      decodeObjectOps,
      failure,
      _.invalidNel[A],
      [X] => (result: ValidatedNel[E, X]) => result.isValid,
      [X] => (decoder: Decoder[F, T, E, X]) => decoder.decodeAccumulating,
      instances
    )

  private[derivation] def decodeSumEither[F[_] : Monad, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[Either[E, A]] =
    decodeSum[F, [X] =>> Either[E, X], T, E, A](
      t,
      configuration,
      decodeObjectOps,
      failure,
      stringOptionDecoder,
      _.asLeft[A],
      _.decode,
      instances
    )

  private[derivation] def decodeSumValidatedNel[F[_] : Monad, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[ValidatedNel[E, A]] =
    decodeSum[F, [X] =>> ValidatedNel[E, X], T, E, A](
      t,
      configuration,
      decodeObjectOps,
      failure,
      stringOptionDecoder,
      _.invalidNel[A],
      _.decodeAccumulating,
      instances
    )

  private[this] def decodeProduct[F[_] : Applicative, G[_] : Applicative, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    asLeft: E => G[A],
    isRight: [X] => G[X] => Boolean,
    decode: [X] => Decoder[F, T, E, X] => T => F[G[X]],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[G[A]] =
    if decodeObjectOps.isObject(t) then
      if configuration.strictDecoding then
        val expectedFields = instances.labels.toList.asInstanceOf[List[String]] ++ configuration.discriminator
        val expectedFieldsSet = expectedFields.toSet
        val unexpectedFields = decodeObjectOps.keys(t).map(_.filterNot(expectedFieldsSet)).getOrElse(Nil)
        if unexpectedFields.nonEmpty then
          asLeft(failure.migrate(UnexpectedFields(t, instances.label, unexpectedFields, expectedFields))).pure[F]
        else handleDecodeProduct(t, configuration, decodeObjectOps, decode.asInstanceOf, isRight, instances)
      else handleDecodeProduct(t, configuration, decodeObjectOps, decode.asInstanceOf, isRight, instances)
    else asLeft(failure.migrate(NotObject(t))).pure[F]

  private[this] def handleDecodeProduct[F[_] : Applicative, G[_] : Applicative, T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    decode: [X] => Decoder[F, T, E, X] => T => F[G[X]],
    isRight: [X] => G[X] => Boolean,
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[G[A]] =
    instances.constructWithLabelDefault[[X] =>> F[G[X]]] {
      [X] => (decoder: Decoder[F, T, E, X], label: String, defaultOpt: Option[X]) =>
        val key = configuration.transformMemberNames(label)
        val current = decodeObjectOps.downField(t, key)
        decode(decoder)(current).map(result => defaultOpt
          .filter(_ => configuration.useDefaults &&
            (!decodeObjectOps.contains(t, key) || (!isRight(result) && decodeObjectOps.isNull(current)))
          )
          .fold(result)(_.pure[G])
        )
    }

  private[this] def decodeSum[F[_] : Monad, G[_], T, E, A](
    t: T,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    stringOptionDecoder: Decoder[F, T, E, Option[String]],
    asLeft: E => G[A],
    decode: Decoder[F, T, E, A] => T => F[G[A]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[G[A]] =
    configuration.discriminator match
      case Some(discriminator) =>
        val current = decodeObjectOps.downField(t, discriminator)
        stringOptionDecoder.decode(current).flatMap {
          case Right(Some(sumTypeName)) =>
            handleDecodeSum[F, G, T, E, A](t, sumTypeName, configuration, decodeObjectOps, failure, asLeft,
              decode.asInstanceOf, instances)
          case Right(None) =>
            asLeft(failure.migrate(NoDiscriminatorField(current, instances.label, discriminator))).pure[F]
          case Left(failure) => asLeft(failure).pure[F]
        }
      case _ =>
        decodeObjectOps.keys(t) match
          case None => asLeft(failure.migrate(NotObject(t))).pure[F]
          case Some(Nil) => asLeft(failure.migrate(NonEmptyObject(t, instances.label))).pure[F]
          case Some(sumTypeName :: tail) =>
            if tail.nonEmpty && configuration.strictDecoding then
              val constructorNames = instances.labels.toList.asInstanceOf[List[String]]
                .map(configuration.transformConstructorNames)
              asLeft(failure.migrate(NotSingleKeyObject(t, instances.label, constructorNames))).pure[F]
            else
              handleDecodeSum[F, G, T, E, A](t, sumTypeName, configuration, decodeObjectOps, failure, asLeft,
                decode.asInstanceOf, instances)
  end decodeSum

  private[this] def handleDecodeSum[F[_] : Applicative, G[_], T, E, A](
    t: T,
    sumTypeName: String,
    configuration: DecoderConfiguration,
    decodeObjectOps: DecodeObjectOps[T],
    failure: Migration[DecodingFailure[T], E],
    asLeft: E => G[A],
    decode: Decoder[F, T, E, A] => T => F[G[A]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): F[G[A]] =
    decodersDict(configuration, instances).get(sumTypeName).fold(
      asLeft(failure.migrate(NoSuchType(t, instances.label, sumTypeName))).pure[F]
    )(decoder => decode(decoder.asInstanceOf[Decoder[F, T, E, A]])(t))

  private[this] def decodersDict[F[_], T, E, A](
    configuration: DecoderConfiguration,
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Map[String, Decoder[F, T, E, ?]] =
    instances.foldRightWithLabel(Map.empty[String, Decoder[F, T, E, ?]]) {
      [X] => (decoder: Decoder[F, T, E, X], label: String, map: Map[String, Decoder[F, T, E, ?]]) =>
        decoder match
          case d: SumDecoder[F, T, E, X] =>
            map ++ decodersDict(d.configuration, d.decoders.asInstanceOf)
          case _ => map + (configuration.transformConstructorNames(label) -> decoder)
    }
end DecoderDerivationInstances

object DecoderDerivationInstances extends DecoderDerivationInstances
