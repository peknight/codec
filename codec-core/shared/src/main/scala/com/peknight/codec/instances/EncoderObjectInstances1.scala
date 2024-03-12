package com.peknight.codec.instances

import cats.Functor
import com.peknight.codec.{Encoder, Object}
import com.peknight.codec.sum.ObjectType

trait EncoderObjectInstances1:
  given objectEncoder[F[_], S, A](using functor: Functor[F], objectType: ObjectType[S], encoder: Encoder[F, Object[S], A])
  : Encoder[F, S, A] =
    Encoder.objectEncoder[F, S, A](encoder)
end EncoderObjectInstances1
