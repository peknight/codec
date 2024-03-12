package com.peknight.codec.instances

import cats.Functor
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType

trait EncoderStringInstances1:
  given stringEncoder[F[_], S, A](using functor: Functor[F], stringType: StringType[S], encoder: Encoder[F, String, A])
  : Encoder[F, S, A] =
    Encoder.stringEncoder[F, S, A](encoder)
end EncoderStringInstances1
