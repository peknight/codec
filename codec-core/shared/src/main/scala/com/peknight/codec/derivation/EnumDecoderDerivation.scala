package com.peknight.codec.derivation

import cats.Functor
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.codec.configuration.Configuration
import com.peknight.codec.error.{DecodingFailure, NoSuchEnum}
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumDecoderDerivation:
  inline def derived[F[_], T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    new EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, _]] =
        enumDecodersDict[F, T, A](this, configuration, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        decodeEnum[F, T, A, generic.Repr](t, configuration, stringDecoder, generic, singletons)
  end derived

  def unsafeDerived[F[_], T, A](using configuration: Configuration)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    new EnumDecoder[F, T, A]:
      def decoders: Map[String, Decoder[F, T, _]] =
        enumDecodersDict[F, T, A](this, configuration, generic)
      def decode(t: T): F[Either[DecodingFailure, A]] =
        unsafeDecodeEnum[F, T, A, generic.Repr](t, configuration, stringDecoder, generic)
  end unsafeDerived

  private[derivation] def decodeEnum[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A],
    singletons: Repr
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) =>
        generic.labels.zip(singletons).toList.asInstanceOf[List[(String, A)]]
          .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
          .map(_._2)
          .fold(NoSuchEnum(caseName).label(generic.label).value(t).asLeft[A])(_.asRight[DecodingFailure])
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def unsafeDecodeEnum[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    configuration: Configuration,
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A],
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) =>
        generic.labels.zip(generic.singletons).toList.asInstanceOf[List[(String, Option[A])]]
          .find(tuple => configuration.transformConstructorNames(tuple._1) == caseName)
          .flatMap(_._2)
          .fold(NoSuchEnum(caseName).label(generic.label).value(t).asLeft[A])(_.asRight[DecodingFailure])
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

end EnumDecoderDerivation
object EnumDecoderDerivation extends EnumDecoderDerivation