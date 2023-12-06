package com.peknight.codec.derivation

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.sum.ObjectType
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait EncoderInstances:
  given derivedEncoder[F[_], S, O, A](using
    configuration: EncoderConfiguration,
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): LowPriority[Encoder[F, S, A]] =
    LowPriority(EncoderDerivation.derived[F, S, O, A])

end EncoderInstances
object EncoderInstances extends EncoderInstances
