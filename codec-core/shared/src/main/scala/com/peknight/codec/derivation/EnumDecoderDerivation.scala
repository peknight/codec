package com.peknight.codec.derivation

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Applicative, Functor, Id, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.config.Config
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

  inline def derived[F[_], T, A](using config: Config)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    show: Show[T],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumDecoderInstance[F, T, A](
      t => decodeEnum[F, T, A](t, config, stringDecoder, generic)(singletons)(_.value(_))
    )(
      self => enumDecodersDict[F, T, A](self, config, generic)
    )
  end derived

  def unsafeDerived[F[_], T, A](using config: Config)(using
    functor: Functor[F],
    stringDecoder: Decoder[F, T, String],
    show: Show[T],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, T, A] =
    enumDecoderInstance[F, T, A](
      t => unsafeDecodeEnum[F, T, A, generic.Repr](t, config, stringDecoder, show, generic)
    )(
      self => enumDecodersDict[F, T, A](self, config, generic)
    )
  end unsafeDerived

  private[derivation] def decodeEnum[F[_]: Functor, T, A](
    t: T,
    config: Config,
    stringDecoder: Decoder[F, T, String],
    generic: Generic.Sum[A]
  )(
    singletons: generic.Repr
  )(mapEnumError: (DecodingFailure, T) => DecodingFailure): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) => 
        stringDecodeEnum[Id, A](caseName, config, generic)(singletons).left.map(mapEnumError(_, t))
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def unsafeDecodeEnum[F[_]: Functor, T, A, Repr <: Tuple](
    t: T,
    config: Config,
    stringDecoder: Decoder[F, T, String],
    show: Show[T],
    generic: Generic.Sum[A],
  ): F[Either[DecodingFailure, A]] =
    stringDecoder.decode(t).map {
      case Right(caseName) =>
        unsafeStringDecodeEnum[Id, A](caseName, config, generic).left.map(_.value(t)(using show))
      case Left(e) => e.asLeft[A]
    }

  private[derivation] def enumDecodersDict[F[_], T, A](
    decoder: Decoder[F, T, A],
    config: Config,
    generic: Generic.Sum[A]
  ): Map[String, Decoder[F, T, A]] =
    generic.labels.toList.asInstanceOf[List[String]]
      .flatMap(label => config.transformConstructorNames(label).map(_ -> decoder).toList)
      .toMap

  inline def derivedStringDecodeEnum[F[_], A](using config: Config)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, String, A] =
    val singletons = summonAllSingletons[generic.Repr](generic.label)
    enumDecoderInstance[F, String, A](
      t => stringDecodeEnum[F, A](t, config, generic)(singletons)
    )(
      self => enumDecodersDict[F, String, A](self, config, generic)
    )
  end derivedStringDecodeEnum

  def unsafeDerivedStringDecodeEnum[F[_], A](using config: Config)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): EnumDecoder[F, String, A] =
    partialDerivedStringDecodeEnum[F, A]()
  end unsafeDerivedStringDecodeEnum

  def partialDerivedStringDecodeEnum[F[_], A](f: String => Option[A] = _ => None)
                                             (using config: Config)
                                             (using applicative: Applicative[F], generic: Generic.Sum[A])
  : EnumDecoder[F, String, A] =
    enumDecoderInstance[F, String, A](
      t => unsafeStringDecodeEnum[F, A](t, config, generic, f)
    )(
      self => enumDecodersDict[F, String, A](self, config, generic)
    )
  end partialDerivedStringDecodeEnum

  private[derivation] def stringDecodeEnum[F[_]: Applicative, A](
    caseName: String,
    config: Config,
    generic: Generic.Sum[A]
  )(singletons: generic.Repr): F[Either[DecodingFailure, A]] =
    generic.labels.zip(singletons).toList.asInstanceOf[List[(String, A)]]
      .find(tuple => config.transformConstructorNames(tuple._1).toList.contains(caseName))
      .map(_._2)
      .toRight(NoSuchEnum(caseName).label(generic.label))
      .pure[F]

  private[derivation] def unsafeStringDecodeEnum[F[_]: Applicative, A](
    caseName: String,
    config: Config,
    generic: Generic.Sum[A],
    f: String => Option[A] = _ => None
  ): F[Either[DecodingFailure, A]] =
    generic.labels.zip(generic.singletons).toList.asInstanceOf[List[(String, Option[A])]]
      .find(tuple => config.transformConstructorNames(tuple._1).toList.contains(caseName))
      .flatMap(_._2)
      .orElse(f(caseName))
      .toRight(NoSuchEnum(caseName).label(generic.label))
      .pure[F]

end EnumDecoderDerivation
object EnumDecoderDerivation extends EnumDecoderDerivation