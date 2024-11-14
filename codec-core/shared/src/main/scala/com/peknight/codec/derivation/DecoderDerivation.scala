package com.peknight.codec.derivation

import cats.data.Validated
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
import com.peknight.generic.tuple.syntax.sequence

trait DecoderDerivation:
  def derived[F[_], S, A](using configuration: DecoderConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    instances: => Generic.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, A](using configuration)(using monad, objectType, nullType, inst),
      inst ?=> derivedSum[F, S, A](using configuration)(using monad, objectType, stringDecoder, stringOptionDecoder,
        inst)
    )

  def derivedProduct[F[_], S, A](using configuration: DecoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    decodeProduct(_, configuration, objectType, nullType, instances)
  end derivedProduct

  def derivedSum[F[_], S, A](using configuration: DecoderConfiguration)(using
    monad: Monad[F],
    objectType: ObjectType[S],
    stringDecoder: Decoder[F, Cursor[S], String],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): Decoder[F, Cursor[S], A] =
    val generic: Generic.Sum[A] = instances.generic
    generic.singletons.sequence match
      case Some(singletons) =>
        new EnumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            EnumDecoderDerivation.enumDecodersDict[F, Cursor[S], A](this, configuration,
              instances.generic)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            EnumDecoderDerivation.decodeEnum(cursor, configuration, stringDecoder, generic)(singletons)
      case _ =>
        new SumDecoder[F, Cursor[S], A]:
          def decoders: Map[String, Decoder[F, Cursor[S], ?]] =
            decodersDict[F, Cursor[S], A](configuration, instances)
          def decode(cursor: Cursor[S]): F[Either[DecodingFailure, A]] =
            decodeSum(cursor, configuration, objectType, stringOptionDecoder, instances)
  end derivedSum

  private[derivation] def decodeProduct[F[_]: Applicative, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    val labels = instances.labels.toList.asInstanceOf[List[String]]
    if cursor.focus.exists(objectType.isObject) then
      if configuration.strictDecoding then
        if configuration.extendedField.exists(labels.contains) then
          handleDecodeProduct(cursor, labels, configuration, objectType, nullType, instances)
        else
          val expectedFields = labels ++ configuration.discriminator
          val expectedFieldsSet = expectedFields.toSet
          val unexpectedFields = cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList)
            .map(_.filterNot(expectedFieldsSet)).getOrElse(Nil)
          if unexpectedFields.nonEmpty then
            UnexpectedFields(unexpectedFields, expectedFields).label(instances.label).cursor(cursor).asLeft[A].pure[F]
          else
            handleDecodeProduct(cursor, labels, configuration, objectType, nullType, instances)
      else
        handleDecodeProduct(cursor, labels, configuration, objectType, nullType, instances)
    else NotObject.cursor(cursor).asLeft[A].pure[F]

  private def handleDecodeProduct[F[_]: Applicative, S, A](
    cursor: Cursor[S],
    labels: List[String],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    instances.constructWithLabelDefault[[X] =>> F[Validated[DecodingFailure, X]]] {
      [X] => (decoder: Decoder[F, Cursor[S], X], label: String, defaultOpt: Option[X]) =>
        val extendedField = configuration.extendedField.contains(label)
        val key = configuration.transformMemberNames(label)
        val current =
          if extendedField then
            val removeFields = (configuration.discriminator ++ labels.map(configuration.transformMemberNames)).toSeq
            cursor.remove(removeFields)(using objectType)
          else cursor.downField(key)(using objectType)
        decoder.decode(current).map(result => defaultOpt
          .filter { _ =>
            if !configuration.useDefaults then
              false
            else if !result.isRight && current.focus.exists(nullType.isNull) then
              true
            else if extendedField then
              !current.focus.exists(objectType.isObject)
            else
              !cursor.focus.exists(s => objectType.asObject(s).exists(o => objectType.contains(o, key)))
          }
          .fold(result.toValidated)(_.valid[DecodingFailure])
        )
    }.map(_.toEither)

  private[derivation] def decodeSum[F[_]: Monad, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    configuration.discriminator match
      case Some(discriminator) =>
        val discriminatorT = cursor.downField(discriminator)(using objectType)
        stringOptionDecoder.decode(discriminatorT).flatMap {
          case Right(Some(sumTypeName)) =>
            decodersDict(configuration, instances).get(sumTypeName) match
              case None =>
                NoSuchType(sumTypeName).label(instances.label).cursor(cursor).asLeft[A].pure[F]
              case Some(e: EnumDecoder[F, Cursor[S], _]) =>
                e.asInstanceOf[Decoder[F, Cursor[S], A]].decode(discriminatorT)
              case Some(decoder) => decoder.asInstanceOf[Decoder[F, Cursor[S], A]].decode(cursor)
          case Right(None) =>
            NoDiscriminatorField(discriminator).label(instances.label).cursor(discriminatorT).asLeft[A].pure[F]
          case Left(failure) => failure.asLeft[A].pure[F]
        }
      case _ =>
        cursor.focus.flatMap(objectType.asObject).map(o => objectType.keys(o).toList) match
          case None => NotObject.cursor(cursor).asLeft[A].pure[F]
          case Some(Nil) => NonEmptyObject.label(instances.label).cursor(cursor).asLeft[A].pure[F]
          case Some(sumTypeName :: tail) =>
            if tail.nonEmpty && configuration.strictDecoding then
              val constructorNames = instances.labels.toList.asInstanceOf[List[String]]
                .map(configuration.transformConstructorNames)
              NotSingleKeyObject(constructorNames).label(instances.label).cursor(cursor).asLeft[A].pure[F]
            else
              val downCursor = cursor.downField(sumTypeName)(using objectType)
              decodersDict(configuration, instances).get(sumTypeName).fold(
                NoSuchType(sumTypeName).label(instances.label).cursor(downCursor).asLeft[A].pure[F]
              )(_.asInstanceOf[Decoder[F, Cursor[S], A]].decode(downCursor))
  end decodeSum

  private[derivation] def decodersDict[F[_], T, A](
    configuration: DecoderConfiguration,
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, T, X], A]
  ): Map[String, Decoder[F, T, ?]] =
    instances.foldRightWithLabel(Map.empty[String, Decoder[F, T, ?]]) {
      [X] => (decoder: Decoder[F, T, X], label: String, map: Map[String, Decoder[F, T, ?]]) =>
        decoder match
          case d: SumDecoder[F, T, X] => map ++ d.decoders
          case d => map + (configuration.transformConstructorNames(label) -> d)
    }
end DecoderDerivation

object DecoderDerivation extends DecoderDerivation
