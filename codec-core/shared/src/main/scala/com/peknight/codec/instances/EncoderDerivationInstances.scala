package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.derivation.EncoderDerivation
import com.peknight.codec.sum.ObjectType
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait EncoderDerivationInstances extends EncoderDerivation:
  given derivedEncoder[F[_], S, A](using
    configuration: EncoderConfiguration,
    applicative: Applicative[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): LowPriority[Encoder[F, S, A]] =
    LowPriority(derived[F, S, A])

end EncoderDerivationInstances
object EncoderDerivationInstances extends EncoderDerivationInstances
