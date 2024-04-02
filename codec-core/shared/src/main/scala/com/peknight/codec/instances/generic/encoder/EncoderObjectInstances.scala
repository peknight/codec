package com.peknight.codec.instances.generic.encoder

import cats.Functor
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Encoder, Object}
import com.peknight.generic.priority.MidPriority

trait EncoderObjectInstances:
  given objectEncoder[F[_], S, A](using functor: Functor[F], objectType: ObjectType[S], encoder: Encoder[F, Object[S], A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.objectEncoder[F, S, A])
end EncoderObjectInstances
object EncoderObjectInstances extends EncoderObjectInstances
