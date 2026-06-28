package com.peknight.codec.instances.generic.encoder

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.config.EncoderConfig
import com.peknight.codec.sum.ObjectType
import com.peknight.generic.Generic
import com.peknight.generic.priority.LowPriority

trait EncoderDerivationInstances:
  given derivedEncoder[F[_], S, A](using
    config: EncoderConfig,
    applicative: Applicative[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): LowPriority[Encoder[F, S, A]] =
    LowPriority(Encoder.derived[F, S, A])

end EncoderDerivationInstances
object EncoderDerivationInstances extends EncoderDerivationInstances
