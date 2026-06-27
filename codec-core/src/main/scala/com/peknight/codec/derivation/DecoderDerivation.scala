package com.peknight.codec.derivation

import cats.data.{NonEmptyList, Validated}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.validated.*
import cats.{Monad, Show}
import com.peknight.cats.instances.applicative.given
import com.peknight.codec.Decoder
import com.peknight.codec.config.DecoderConfig
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.*
import com.peknight.codec.reader.Key
import com.peknight.codec.sum.{NullType, ObjectType}
import com.peknight.generic.Generic
import com.peknight.generic.tuple.syntax.sequence

trait DecoderDerivation:
  def derived[F[_], S, A](using config: DecoderConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, A](using config)(using monad, objectType, nullType, show, inst),
      inst ?=> derivedSum[F, S, A](using config)(using monad, objectType, stringDecoder, stringOptionDecoder, show, inst)
    )

  def derivedProduct[F[_], S, A](using config: DecoderConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    show: Show[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    decodeProduct(_, config, objectType, nullType, show, instances)
  end derivedProduct

  def derivedSum[F[_], S, A](using config: DecoderConfig)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    handleDerivedSum[F, Cursor[S], A](decodeSum(_, config, objectType, stringOptionDecoder, show, instances))(_.cursor(_))

  def derivedByKey[F[_], A](using config: DecoderConfig)(using
    monad: Monad[F],
    stringDecoder: Decoder[F, Key, String],
    stringOptionDecoder: Decoder[F, Key, Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, Key, X], A]
  ): Decoder[F, Key, A] =
    instances.derive(
      inst ?=> derivedProductByKey[F, A](using config)(using monad, inst),
      inst ?=> derivedSumByKey[F, A](using config)(using monad, stringDecoder, stringOptionDecoder, inst)
    )

  def derivedProductByKey[F[_], A](using config: DecoderConfig)(using
    monad: Monad[F], instances: => Generic.Product.Instances[[X] =>> Decoder[F, Key, X], A]
  ): Decoder[F, Key, A] =
    case Key(keys) => instances.constructWithLabelDefault[[X] =>> F[Validated[DecodingFailure, X]]] {
      [X] => (decoder: Decoder[F, Key, X], label: String, defaultOpt: Option[X]) =>
        val ks = config.transformMemberNames(label).map(key => Key(keys :+ key))
        decodeProductMember[F, Key, X](ks, decoder, defaultOpt, false, true, config)(_ => false)(_ => true)(true)
    }.map(_.toEither)

  def derivedSumByKey[F[_], A](using config: DecoderConfig)(using
    monad: Monad[F],
    stringDecoder: Decoder[F, Key, String],
    stringOptionDecoder: Decoder[F, Key, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Key, X], A]
  ): Decoder[F, Key, A] =
    handleDerivedSum[F, Key, A](decodeSumByKey(_, config, stringOptionDecoder, instances))(_.value(_))

  private[derivation] def decodeProduct[F[_]: Monad, S, A](
    cursor: Cursor[S],
    config: DecoderConfig,
    objectType: ObjectType[S],
    nullType: NullType[S],
    show: Show[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    val labels = instances.labels.toList.asInstanceOf[List[String]]
    if cursor.focus.exists(objectType.isObject) then
      if config.strictDecoding then
        if config.extField.exists(labels.contains) then
          handleDecodeProduct(cursor, labels, config, objectType, nullType, instances)
        else
          val expectedFields = labels ++ config.discriminator
          val expectedFieldsSet = expectedFields.toSet
          val unexpectedFields = cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList)
            .map(_.filterNot(expectedFieldsSet)).getOrElse(Nil)
          if unexpectedFields.nonEmpty then
            UnexpectedFields(unexpectedFields, expectedFields).label(instances.label)
              .cursor(cursor)(using show).asLeft[A].pure[F]
          else
            handleDecodeProduct(cursor, labels, config, objectType, nullType, instances)
      else
        handleDecodeProduct(cursor, labels, config, objectType, nullType, instances)
    else NotObject.cursor(cursor)(using show).asLeft[A].pure[F]

  private def handleDecodeProduct[F[_]: Monad, S, A](
    cursor: Cursor[S],
    labels: List[String],
    config: DecoderConfig,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    instances.constructWithLabelDefault[[X] =>> F[Validated[DecodingFailure, X]]] {
      [X] => (decoder: Decoder[F, Cursor[S], X], label: String, defaultOpt: Option[X]) =>
        val extField = config.extField.contains(label)
        val keys = config.transformMemberNames(label)
        val cursors =
          if extField then
            val removeFields = config.discriminator.toList :::
              labels.flatMap(label => config.transformMemberNames(label).toList)
            NonEmptyList.one(cursor.remove(removeFields)(using objectType))
          else keys.map(key => cursor.downField(key)(using objectType))
        decodeProductMember[F, Cursor[S], X](cursors, decoder, defaultOpt, extField, false, config)(
          _.focus.exists(nullType.isNull)
        )(
          _.focus.flatMap(objectType.asObject).exists(objectType.isEmpty)
        )(
          cursor.focus.flatMap(objectType.asObject).exists(o => keys.exists(key => objectType.contains(o, key)))
        )
    }.map(_.toEither)

  private def decodeProductMember[F[_]: Monad, T, X](ts: NonEmptyList[T], decoder: Decoder[F, T, X],
                                                     defaultOpt: Option[X], extField: Boolean, readByKey: Boolean,
                                                     config: DecoderConfig)
                                                    (isNull: T => Boolean)(isExtEmpty: T => Boolean)
                                                    (keyExists: => Boolean)
  : F[Validated[DecodingFailure, X]] =
    given CanEqual[None.type, X] = CanEqual.derived
    Monad[F].tailRecM[(List[T], Option[(T, Either[DecodingFailure, X])]), (T, Either[DecodingFailure, X])](
      (ts.toList, None)
    ) {
      case (head :: tail, result) => decoder.decode(head).map {
        case Left(error) => (tail, result.getOrElse((head, error.asLeft)).some).asLeft
        case rn@Right(None) => (tail, (head, rn).some).asLeft
        case r => (head, r).asRight
      }
      case (_, result) => result.get.asRight.pure
    }.map { case (current, result) =>
      (current, result, defaultOpt) match
        case (_, result, None) => result.toValidated
        case (_, result, _) if !config.useDefaults => result.toValidated
        case (current, Left(error), Some(d)) if isNull(current) || DecodingFailure.isNullError(error) =>
          d.valid[DecodingFailure]
        case (current, result, Some(d)) if extField =>
          if isExtEmpty(current) then d.valid[DecodingFailure] else result.toValidated
        case (_, Right(None), Some(d)) if readByKey => d.valid[DecodingFailure]
        case (_, result, Some(d)) => if keyExists then result.toValidated else d.valid[DecodingFailure]
    }

  private def handleDerivedSum[F[_], T, A](decodeSum: T => F[Either[DecodingFailure, A]])
                                          (mapEnumError: (DecodingFailure, T) => DecodingFailure)
                                          (using config: DecoderConfig)
                                          (using monad: Monad[F], stringDecoder: Decoder[F, T, String],
                                           stringOptionDecoder: Decoder[F, T, Option[String]],
                                           instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, X], A])
  : Decoder[F, T, A] =
    val generic: Generic.Sum[A] = instances.generic
    generic.singletons.sequence match
      case Some(singletons) =>
        new EnumDecoder[F, T, A]:
          def decoders: Map[String, Decoder[F, T, ?]] =
            EnumDecoderDerivation.enumDecodersDict[F, T, A](this, config, instances.generic)
          def decode(t: T): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnum(t, config, stringDecoder, generic)(singletons)(mapEnumError)
      case _ =>
        new SumDecoder[F, T, A]:
          def decoders: Map[String, Decoder[F, T, ?]] =
            decodersDict[F, T, A](config, instances)

          def decode(t: T): F[Either[DecodingFailure, A]] =
            decodeSum(t)
  end handleDerivedSum

  private[derivation] def decodeSum[F[_]: Monad, S, A](
    cursor: Cursor[S],
    config: DecoderConfig,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    show: Show[S],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    config.discriminator match
      case Some(discriminator) =>
        val discriminatorT = cursor.downField(discriminator)(using objectType)
        decodeSumWithDiscriminator[F, Cursor[S], A](cursor, discriminator, discriminatorT, config,
          stringOptionDecoder, instances)(_.cursor(_)(using show))
      case _ =>
        cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList) match
          case None => NotObject.cursor(cursor)(using show).asLeft[A].pure[F]
          case Some(Nil) => NonEmptyObject.label(instances.label).cursor(cursor)(using show).asLeft[A].pure[F]
          case Some(sumTypeName :: tail) =>
            if tail.nonEmpty && config.strictDecoding then
              val constructorNames = instances.labels.toList.asInstanceOf[List[String]]
                .flatMap(label => config.transformConstructorNames(label).toList)
              NotSingleKeyObject(constructorNames).label(instances.label).cursor(cursor)(using show).asLeft[A].pure[F]
            else
              val downCursor = cursor.downField(sumTypeName)(using objectType)
              decodersDict(config, instances).get(sumTypeName).fold(
                NoSuchType(sumTypeName).label(instances.label).cursor(downCursor)(using show).asLeft[A].pure[F]
              )(_.asInstanceOf[Decoder[F, Cursor[S], A]].decode(downCursor))
  end decodeSum

  private def decodeSumByKey[F[_]: Monad, A](
    key: Key,
    config: DecoderConfig,
    stringOptionDecoder: Decoder[F, Key, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Key, X], A]
  ): F[Either[DecodingFailure, A]] =
    config.discriminator match
      case Some(discriminator) =>
        val discriminatorT = Key(key.keys :+ discriminator)
        decodeSumWithDiscriminator[F, Key, A](key, discriminator, discriminatorT, config, stringOptionDecoder,
          instances)(_.value(_))
      case _ => NoDiscriminator.label(instances.label).value(key).asLeft[A].pure[F]
  end decodeSumByKey

  private def decodeSumWithDiscriminator[F[_]: Monad, T, A](
    t: T,
    discriminator: String,
    discriminatorT: T,
    config: DecoderConfig,
    stringOptionDecoder: Decoder[F, T, Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, X], A]
  )(liftError: (DecodingFailure, T) => DecodingFailure): F[Either[DecodingFailure, A]] =
    stringOptionDecoder.decode(discriminatorT).flatMap {
      case Right(sumTypeNameOption) =>
        sumTypeNameOption.orElse(config.sumTypeOnNone) match
          case Some(sumTypeName) =>
            val dict = decodersDict(config, instances)
            dict.get(sumTypeName).orElse(config.sumTypeOnNone.flatMap(dict.get)) match
              case None =>
                liftError(NoSuchType(sumTypeName).label(instances.label), t).asLeft[A].pure[F]
              case Some(e: EnumDecoder[F, T, _]) =>
                e.asInstanceOf[Decoder[F, T, A]].decode(discriminatorT)
              case Some(decoder) => decoder.asInstanceOf[Decoder[F, T, A]].decode(t)
          case _ =>
            liftError(NoDiscriminatorField(discriminator).label(instances.label), discriminatorT).asLeft[A].pure[F]
      case Left(failure) => failure.asLeft[A].pure[F]
    }

  private[derivation] def decodersDict[F[_], T, A](
    config: DecoderConfig,
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, X], A]
  ): Map[String, Decoder[F, T, ?]] =
    instances.foldRightWithLabel(Map.empty[String, Decoder[F, T, ?]]) {
      [X] => (decoder: Decoder[F, T, X], label: String, map: Map[String, Decoder[F, T, ?]]) =>
        decoder match
          case d: SumDecoder[F, T, X] => map ++ d.decoders
          case d => map ++ config.transformConstructorNames(label).map(_ -> d).toList.toMap
    }
end DecoderDerivation
object DecoderDerivation extends DecoderDerivation
