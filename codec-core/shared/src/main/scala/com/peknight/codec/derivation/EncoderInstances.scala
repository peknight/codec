package com.peknight.codec.derivation

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait EncoderInstances:
  given derivedEncoder[F[_], S, A](using
    configuration: EncoderConfiguration,
    applicative: Applicative[F],
    encodeObject: EncodeObjectOps[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): LowPriority[Encoder[F, S, A]] =
    LowPriority(EncoderDerivationInstances.derived[F, S, A])

end EncoderInstances
object EncoderInstances extends EncoderInstances
