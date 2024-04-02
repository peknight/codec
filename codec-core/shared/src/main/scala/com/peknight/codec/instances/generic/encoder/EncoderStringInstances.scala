package com.peknight.codec.instances.generic.encoder

import cats.Functor
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.MidPriority

trait EncoderStringInstances:
  given stringEncoder[F[_], S, A](using functor: Functor[F], stringType: StringType[S], encoder: Encoder[F, String, A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.stringEncoder[F, S, A])
end EncoderStringInstances
object EncoderStringInstances extends EncoderStringInstances
