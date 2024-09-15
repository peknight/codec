package com.peknight.codec.derivation

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Functor, Id}
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.Configuration
import com.peknight.codec.error.{DecodingFailure, NoSuchEnum}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumDecoderDerivation:
  def enumDecoderInstance[F[_], T, A](decode0: T => F[Either[DecodingFailure, A]])
                                     (decoders0: Decoder[F, T, A] => Map[String, Decoder[F, T, ?]])
  : EnumDecoder[F, T, A] =
    new EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, ?]] = decoders0(this)
      def decode(t: T): F[Either[DecodingFailure, A]] = decode0(t)
  end enumDecoderInstance

  inline def derived[F[_], T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumDecoderInstance[F, T, A](
      t => decodeEnum[F, T, A](t, configuration, stringDecoder, generic)(singletons)
    )(
      self => enumDecodersDict[F, T, A](self, configuration, generic)
    )
  end derived

  def unsafeDerived[F[_], T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    enumDecoderInstance[F, T, A](
      t => unsafeDecodeEnum[F, T, A, generic.Repr](t, configuration, stringDecoder, generic)
    )(
      self => enumDecodersDict[F, T, A](self, configuration, generic)
    )
  end unsafeDerived

  private[derivation] def decodeEnum[F[_]: Functor, T, A](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  )(
    singletons: generic.Repr
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) => stringDecodeEnum[Id, A](caseName, configuration, generic)(singletons).left.map(_.value(t))
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def unsafeDecodeEnum[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A],
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) => unsafeStringDecodeEnum[Id, A](caseName, configuration, generic).left.map(_.value(t))
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def enumDecodersDict[F[_], T, A](
    decoder: Decoder[F, T, A],
    configuration: Configuration,
    generic: Generic.Sum[A]
  ): Map[String, Decoder[F, T, A]] =
    generic.labels.toList.asInstanceOf[List[String]]
      .map(label => (configuration.transformConstructorNames(label), decoder))
      .toMap

  inline def derivedStringDecodeEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, String, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumDecoderInstance[F, String, A](
      t => stringDecodeEnum[F, A](t, configuration, generic)(singletons)
    )(
      self => enumDecodersDict[F, String, A](self, configuration, generic)
    )
  end derivedStringDecodeEnum

  def unsafeDerivedStringDecodeEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, String, A] =
    enumDecoderInstance[F, String, A](
      t => unsafeStringDecodeEnum[F, A](t, configuration, generic)
    )(
      self => enumDecodersDict[F, String, A](self, configuration, generic)
    )
  end unsafeDerivedStringDecodeEnum

  private[derivation] def stringDecodeEnum[F[_]: Applicative, A](
    caseName: String,
    configuration: Configuration,
    generic: Generic.Sum[A]
  )(singletons: generic.Repr): F[Either[DecodingFailure, A]] =
    generic.labels.zip(singletons).toList.asInstanceOf[List[(String, A)]]
      .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
      .map(_._2)
      .toRight(NoSuchEnum(caseName).label(generic.label))
      .pure[F]

  private[derivation] def unsafeStringDecodeEnum[F[_]: Applicative, A](
    caseName: String,
    configuration: Configuration,
    generic: Generic.Sum[A]
  ): F[Either[DecodingFailure, A]] =
    generic.labels.zip(generic.singletons).toList.asInstanceOf[List[(String, Option[A])]]
      .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
      .flatMap(_._2)
      .toRight(NoSuchEnum(caseName).label(generic.label))
      .pure[F]

end EnumDecoderDerivation
object EnumDecoderDerivation extends EnumDecoderDerivation