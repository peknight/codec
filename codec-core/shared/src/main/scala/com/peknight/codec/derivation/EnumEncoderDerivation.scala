package com.peknight.codec.derivation

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
    (a: A) => encodeEnum(a, configuration, stringEncoder, generic)

  private[derivation] def encodeEnum[F[_], S, A](
    a: A,
    configuration: Configuration,
    stringEncoder: Encoder[F, S, String],
    generic: Generic.Sum[A]
  ): F[S] =
    stringEncoder.encode(configuration.transformConstructorNames(generic.label(a)))
end EnumEncoderDerivation
object EnumEncoderDerivation extends EnumEncoderDerivation
