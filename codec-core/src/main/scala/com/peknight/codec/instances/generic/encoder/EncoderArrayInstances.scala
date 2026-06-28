package com.peknight.codec.instances.generic.encoder

import cats.Functor
import com.peknight.codec.Encoder
import com.peknight.codec.sum.ArrayType
import com.peknight.generic.priority.MidPriority

trait EncoderArrayInstances:
  given encodeAM[F[_], S, A](using functor: Functor[F], arrayType: ArrayType[S], encoder: Encoder[F, Vector[S], A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.encodeA[F, S, A])
end EncoderArrayInstances
object EncoderArrayInstances extends EncoderArrayInstances
