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
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.*
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.migration.id.Migration
import com.peknight.generic.tuple.syntax.sequence

trait DecoderDerivation:
  def derived[F[_], S, A](using configuration: DecoderConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], DecodingFailure, String],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Decoder[F, Cursor[S], DecodingFailure, A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, A](configuration, objectType, nullType, inst),
      inst ?=> derivedSum[F, S, A](configuration, objectType, stringDecoder,
        stringOptionDecoder, inst)
    )

  private[this] def derivedProduct[F[_]: Applicative, S, A](
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Decoder[F, Cursor[S], DecodingFailure, A] =
    new Decoder[F, Cursor[S], DecodingFailure, A]:
      def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
        decodeProductEither(cursor, configuration, objectType, nullType, instances)
      def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
        decodeProductValidatedNel(cursor, configuration, objectType, nullType, instances)
  end derivedProduct

  private[this] def derivedSum[F[_]: Monad, S, A](
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringDecoder: Decoder[F, Cursor[S], DecodingFailure, String],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): Decoder[F, Cursor[S], DecodingFailure, A] =
    instances.singletons.sequence match
      case Some(singletons) =>
        new EnumDecoder[F, Cursor[S], DecodingFailure, A]:
          def decoders: Map[String, Decoder[F, Cursor[S], DecodingFailure, _]] =
            EnumDecoderDerivation.enumDecodersDict[F, Cursor[S], DecodingFailure, A](this, configuration,
              instances.generic)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnumEither(cursor, configuration, stringDecoder, instances.generic,
              singletons)
          def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnumValidatedNel(cursor, configuration, stringDecoder, instances.generic,
              singletons)
      case _ =>
        new SumDecoder[F, Cursor[S], DecodingFailure, A]:
          def decoders: Map[String, Decoder[F, Cursor[S], DecodingFailure, _]] =
            decodersDict[F, Cursor[S], DecodingFailure, A](configuration, instances)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            decodeSumEither(cursor, configuration, objectType, stringOptionDecoder, instances)
          def decodeAccumulating(cursor: Cursor[S]): F[ValidatedNel[DecodingFailure, A]] =
            decodeSumValidatedNel(cursor, configuration, objectType, stringOptionDecoder, instances)
  end derivedSum

  private[derivation] def decodeProductEither[F[_]: Applicative, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[Either[DecodingFailure, A]] =
    decodeProduct[F, [X] =>> Either[DecodingFailure, X], S, A](
      cursor,
      configuration,
      objectType,
      nullType,
      _.asLeft[A],
      [X] => (result: Either[DecodingFailure, X]) => result.isRight,
      [X] => (decoder: Decoder[F, Cursor[S], DecodingFailure, X]) => decoder.decode,
      instances
    )

  private[derivation] def decodeProductValidatedNel[F[_]: Applicative, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[ValidatedNel[DecodingFailure, A]] =
    decodeProduct[F, [X] =>> ValidatedNel[DecodingFailure, X], S, A](
      cursor,
      configuration,
      objectType,
      nullType,
      _.invalidNel[A],
      [X] => (result: ValidatedNel[DecodingFailure, X]) => result.isValid,
      [X] => (decoder: Decoder[F, Cursor[S], DecodingFailure, X]) => decoder.decodeAccumulating,
      instances
    )

  private[derivation] def decodeSumEither[F[_]: Monad, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[Either[DecodingFailure, A]] =
    decodeSum[F, [X] =>> Either[DecodingFailure, X], S, A](
      cursor,
      configuration,
      objectType,
      stringOptionDecoder,
      _.asLeft[A],
      _.decode,
      instances
    )

  private[derivation] def decodeSumValidatedNel[F[_]: Monad, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[ValidatedNel[DecodingFailure, A]] =
    decodeSum[F, [X] =>> ValidatedNel[DecodingFailure, X], S, A](
      cursor,
      configuration,
      objectType,
      stringOptionDecoder,
      _.invalidNel[A],
      _.decodeAccumulating,
      instances
    )

  private[this] def decodeProduct[F[_]: Applicative, G[_]: Applicative, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    asLeft: DecodingFailure => G[A],
    isRight: [X] => G[X] => Boolean,
    decode: [X] => Decoder[F, Cursor[S], DecodingFailure, X] => Cursor[S] => F[G[X]],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[G[A]] =
    if cursor.focus.exists(objectType.isObject) then
      if configuration.strictDecoding then
        val expectedFields = instances.labels.toList.asInstanceOf[List[String]] ++ configuration.discriminator
        val expectedFieldsSet = expectedFields.toSet
        val unexpectedFields = cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList)
          .map(_.filterNot(expectedFieldsSet)).getOrElse(Nil)
        if unexpectedFields.nonEmpty then
          asLeft(UnexpectedFields(unexpectedFields, expectedFields).label(instances.label).cursor(cursor)).pure[F]
        else
          handleDecodeProduct(cursor, configuration, objectType, nullType, decode.asInstanceOf, isRight, instances)
      else
        handleDecodeProduct(cursor, configuration, objectType, nullType, decode.asInstanceOf, isRight, instances)
    else asLeft(NotObject.cursor(cursor)).pure[F]

  private[this] def handleDecodeProduct[F[_]: Applicative, G[_]: Applicative, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    decode: [X] => Decoder[F, Cursor[S], DecodingFailure, X] => Cursor[S] => F[G[X]],
    isRight: [X] => G[X] => Boolean,
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[G[A]] =
    instances.constructWithLabelDefault[[X] =>> F[G[X]]] {
      [X] => (decoder: Decoder[F, Cursor[S], DecodingFailure, X], label: String, defaultOpt: Option[X]) =>
        val key = configuration.transformMemberNames(label)
        val current = cursor.downField(key)(using objectType)
        decode(decoder)(current).map(result => defaultOpt
          .filter(_ => configuration.useDefaults &&
            (!cursor.focus.exists(s => objectType.asObject(s).exists(o => objectType.contains(o, key))) ||
              (!isRight(result) && current.focus.exists(nullType.isNull)))
          )
          .fold(result)(_.pure[G])
        )
    }

  private[this] def decodeSum[F[_]: Monad, G[_], S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], DecodingFailure, Option[String]],
    asLeft: DecodingFailure => G[A],
    decode: Decoder[F, Cursor[S], DecodingFailure, A] => Cursor[S] => F[G[A]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], DecodingFailure, X], A]
  ): F[G[A]] =
    configuration.discriminator match
      case Some(discriminator) =>
        val discriminatorT = cursor.downField(discriminator)(using objectType)
        stringOptionDecoder.decode(discriminatorT).flatMap {
          case Right(Some(sumTypeName)) =>
            decodersDict(configuration, instances).get(sumTypeName) match
              case None =>
                asLeft(NoSuchType(sumTypeName).label(instances.label).cursor(cursor)).pure[F]
              case Some(e: EnumDecoder[F, Cursor[S], DecodingFailure, _]) =>
                decode(e.asInstanceOf[Decoder[F, Cursor[S], DecodingFailure, A]])(discriminatorT)
              case Some(decoder) => decode(decoder.asInstanceOf[Decoder[F, Cursor[S], DecodingFailure, A]])(cursor)
          case Right(None) =>
            asLeft(NoDiscriminatorField(discriminator).label(instances.label).cursor(discriminatorT)).pure[F]
          case Left(failure) => asLeft(failure).pure[F]
        }
      case _ =>
        cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList) match
          case None => asLeft(NotObject.cursor(cursor)).pure[F]
          case Some(Nil) => asLeft(NonEmptyObject.label(instances.label).cursor(cursor)).pure[F]
          case Some(sumTypeName :: tail) =>
            if tail.nonEmpty && configuration.strictDecoding then
              val constructorNames = instances.labels.toList.asInstanceOf[List[String]]
                .map(configuration.transformConstructorNames)
              asLeft(NotSingleKeyObject(constructorNames).label(instances.label).cursor(cursor)).pure[F]
            else
              val downCursor = cursor.downField(sumTypeName)(using objectType)
              decodersDict(configuration, instances).get(sumTypeName).fold(
                asLeft(NoSuchType(sumTypeName).label(instances.label).cursor(downCursor)).pure[F]
              )(decoder => decode(decoder.asInstanceOf[Decoder[F, Cursor[S], DecodingFailure, A]])(downCursor))
  end decodeSum

  private[derivation] def decodersDict[F[_], T, E, A](
    configuration: DecoderConfiguration,
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, E, X], A]
  ): Map[String, Decoder[F, T, E, _]] =
    instances.foldRightWithLabel(Map.empty[String, Decoder[F, T, E, _]]) {
      [X] => (decoder: Decoder[F, T, E, X], label: String, map: Map[String, Decoder[F, T, E, _]]) =>
        decoder match
          case d: SumDecoder[F, T, E, X] => map ++ d.decoders
          case d => map + (configuration.transformConstructorNames(label) -> d)
    }
end DecoderDerivation

object DecoderDerivation extends DecoderDerivation
