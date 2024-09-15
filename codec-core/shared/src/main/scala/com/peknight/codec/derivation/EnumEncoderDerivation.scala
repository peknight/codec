package com.peknight.codec.derivation

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.Configuration
import com.peknight.generic.Generic
import com.peknight.generic.compiletime.summonAllSingletons

trait EnumEncoderDerivation:
  inline def derived[F[_], S, A](using configuration: Configuration)(using
    stringEncoder: Encoder[F, S, String],
    generic: Generic.Sum[A]
  ): Encoder[F, S, A] =
    // Only used to validate if all cases are singletons
    summonAllSingletons[generic.Repr](generic.label)
    Encoder.instance[F, S, A](a => encodeEnum(a, configuration, stringEncoder, generic))

  def unsafeDerived[F[_], S, A](using configuration: Configuration)
                               (using stringEncoder: Encoder[F, S, String], generic: Generic.Sum[A]): Encoder[F, S, A] =
    Encoder.instance[F, S, A](a => encodeEnum(a, configuration, stringEncoder, generic))

  private[derivation] def encodeEnum[F[_], S, A](
    a: A,
    configuration: Configuration,
    stringEncoder: Encoder[F, S, String],
    generic: Generic.Sum[A]
  ): F[S] =
    stringEncoder.encode(configuration.transformConstructorNames(generic.label(a)))

  inline def derivedStringEncodeEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Encoder[F, String, A] =
    // Only used to validate if all cases are singletons
    summonAllSingletons[generic.Repr](generic.label)
    Encoder.instance[F, String, A](a => stringEncodeEnum[F, A](a, configuration, applicative, generic))

  def unsafeDerivedStringEncodeEnum[F[_], A](using configuration: Configuration)(using
    applicative: Applicative[F],
    generic: Generic.Sum[A]
  ): Encoder[F, String, A] =
    Encoder.instance[F, String, A](a => stringEncodeEnum[F, A](a, configuration, applicative, generic))

  private[derivation] def stringEncodeEnum[F[_], A](a: A, configuration: Configuration, applicative: Applicative[F],
                                                    generic: Generic.Sum[A]): F[String] =
    applicative.pure(configuration.transformConstructorNames(generic.label(a)))

end EnumEncoderDerivation
object EnumEncoderDerivation extends EnumEncoderDerivation
