package com.peknight.codec.instances.generic.encoder

import cats.Functor
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.MidPriority

trait EncoderValueInstances1:
  given encodeSM[F[_], S, A](using functor: Functor[F], stringType: StringType[S], encoder: Encoder[F, String, A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.encodeS[F, S, A])
end EncoderValueInstances1
