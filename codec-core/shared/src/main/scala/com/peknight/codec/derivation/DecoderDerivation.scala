package com.peknight.codec.derivation

import cats.data.{NonEmptyList, Validated}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.validated.*
import cats.{Monad, Show}
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
    monad: Monad[F],
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

  private[derivation] def decodeProduct[F[_]: Monad, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    given Show[S] = Show.fromToString
    val labels = instances.labels.toList.asInstanceOf[List[String]]
    if cursor.focus.exists(objectType.isObject) then
      if configuration.strictDecoding then
        if configuration.extField.exists(labels.contains) then
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

  private def handleDecodeProduct[F[_]: Monad, S, A](
    cursor: Cursor[S],
    labels: List[String],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    nullType: NullType[S],
    instances: => Generic.Product.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    instances.constructWithLabelDefault[[X] =>> F[Validated[DecodingFailure, X]]] {
      [X] => (decoder: Decoder[F, Cursor[S], X], label: String, defaultOpt: Option[X]) =>
        val extField = configuration.extField.contains(label)
        val keys = configuration.transformMemberNames(label)
        val cursors =
          if extField then
            val removeFields = configuration.discriminator.toList :::
              labels.flatMap(label => configuration.transformMemberNames(label).toList)
            NonEmptyList.one(cursor.remove(removeFields)(using objectType))
          else keys.map(key => cursor.downField(key)(using objectType))
        given CanEqual[None.type, X] = CanEqual.derived
        Monad[F].tailRecM[(List[Cursor[S]], Option[(Cursor[S], Either[DecodingFailure, X])]), (Cursor[S], Either[DecodingFailure, X])](
          (cursors.toList, None)
        ) {
          case (head :: tail, result) => decoder.decode(head).map {
            case Left(error) => (tail, result.getOrElse((head, error.asLeft)).some).asLeft
            case rn @ Right(None) => (tail, (head, rn).some).asLeft
            case r => (head, r).asRight
          }
          case (_, result) => result.get.asRight.pure
        }.map { case (current, result) =>
          (current, result, defaultOpt) match
            case (_, result, None) => result.toValidated
            case (_, result, _) if !configuration.useDefaults => result.toValidated
            case (current, Left(_), Some(d)) if current.focus.exists(nullType.isNull) => d.valid[DecodingFailure]
            case (current, result, Some(d)) if extField =>
              if current.focus.flatMap(objectType.asObject).exists(objectType.isEmpty) then d.valid[DecodingFailure]
              else result.toValidated
            case (_, result, Some(d)) =>
              val keyExists = cursor.focus.flatMap(objectType.asObject).exists(o => keys.exists(key =>
                objectType.contains(o, key)
              ))
              if keyExists then result.toValidated else d.valid[DecodingFailure]
        }
    }.map(_.toEither)

  private[derivation] def decodeSum[F[_]: Monad, S, A](
    cursor: Cursor[S],
    configuration: DecoderConfiguration,
    objectType: ObjectType[S],
    stringOptionDecoder: Decoder[F, Cursor[S], Option[String]],
    instances: => Generic.Sum.Instances[[X] =>> Decoder[F, Cursor[S], X], A]
  ): F[Either[DecodingFailure, A]] =
    given Show[S] = Show.fromToString
    configuration.discriminator match
      case Some(discriminator) =>
        val discriminatorT = cursor.downField(discriminator)(using objectType)
        stringOptionDecoder.decode(discriminatorT).flatMap {
          case Right(sumTypeNameOption) =>
            sumTypeNameOption.orElse(configuration.sumTypeOnNone) match
              case Some(sumTypeName) =>
                val dict = decodersDict(configuration, instances)
                dict.get(sumTypeName).orElse(configuration.sumTypeOnNone.flatMap(dict.get)) match
                  case None =>
                    NoSuchType(sumTypeName).label(instances.label).cursor(cursor).asLeft[A].pure[F]
                  case Some(e: EnumDecoder[F, Cursor[S], _]) =>
                    e.asInstanceOf[Decoder[F, Cursor[S], A]].decode(discriminatorT)
                  case Some(decoder) => decoder.asInstanceOf[Decoder[F, Cursor[S], A]].decode(cursor)
              case _ =>
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
                .flatMap(label => configuration.transformConstructorNames(label).toList)
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
          case d => map ++ configuration.transformConstructorNames(label).map(_ -> d).toList.toMap
    }
end DecoderDerivation

object DecoderDerivation extends DecoderDerivation
