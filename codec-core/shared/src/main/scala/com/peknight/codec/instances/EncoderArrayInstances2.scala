package com.peknight.codec.instances

import cats.Functor
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType

trait EncoderArrayInstances2:
  given arrayEncoder[F[_], S, A](using functor: Functor[F], arrayType: ArrayType[S], encoder: Encoder[F, Vector[S], A])
  : Encoder[F, S, A] =
    Encoder.arrayEncoder[F, S, A](encoder)
end EncoderArrayInstances2
