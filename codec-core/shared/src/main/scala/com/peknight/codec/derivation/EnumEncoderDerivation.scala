package com.peknight.codec.derivation

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.config.Config
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumEncoderDerivation:
  inline def derived[F[_], S, A](using config: Config)(using
    stringEncoder: Encoder[F, S, String],
    generic: Generic.Sum[A]
  ): Encoder[F, S, A] =
    // Only used to validate if all cases are singletons
    summonAllSingletons[generic.Repr](generic.label)
    Encoder.instance[F, S, A](a => encodeEnum(a, config, stringEncoder, generic))

  def unsafeDerived[F[_], S, A](using config: Config)
                               (using stringEncoder: Encoder[F, S, String], generic: Generic.Sum[A]): Encoder[F, S, A] =
    Encoder.instance[F, S, A](a => encodeEnum(a, config, stringEncoder, generic))

  private[derivation] def encodeEnum[F[_], S, A](
    a: A,
    config: Config,
    stringEncoder: Encoder[F, S, String],
    generic: Generic.Sum[A]
  ): F[S] =
    stringEncoder.encode(config.transformConstructorNames(generic.label(a)).head)

  inline def derivedStringEncodeEnum[F[_], A](using config: Config)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Encoder[F, String, A] =
    // Only used to validate if all cases are singletons
    summonAllSingletons[generic.Repr](generic.label)
    Encoder.instance[F, String, A](a => stringEncodeEnum[F, A](a, config, applicative, generic))

  def unsafeDerivedStringEncodeEnum[F[_], A](using config: Config)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Encoder[F, String, A] =
    partialDerivedStringEncodeEnum[F, A]()

  def partialDerivedStringEncodeEnum[F[_], A](f: PartialFunction[A, String] = PartialFunction.empty)
                                             (using config: Config)
                                             (using applicative: Applicative[F], generic: Generic.Sum[A])
  : Encoder[F, String, A] =
    Encoder.instance[F, String, A](a => stringEncodeEnum[F, A](a, config, applicative, generic, f))

  private[derivation] def stringEncodeEnum[F[_], A](a: A, config: Config, applicative: Applicative[F],
                                                    generic: Generic.Sum[A],
                                                    f: PartialFunction[A, String] = PartialFunction.empty): F[String] =
    applicative.pure(if f.isDefinedAt(a) then f(a) else config.transformConstructorNames(generic.label(a)).head)
end EnumEncoderDerivation
object EnumEncoderDerivation extends EnumEncoderDerivation
