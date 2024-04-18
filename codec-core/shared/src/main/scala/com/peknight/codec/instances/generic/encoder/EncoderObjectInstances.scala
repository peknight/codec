package com.peknight.codec.instances.generic.encoder

import cats.Functor
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.Encoder
import com.peknight.codec.obj.Object
import com.peknight.generic.priority.MidPriority

trait EncoderObjectInstances:
  given encodeOM[F[_], S, A](using functor: Functor[F], objectType: ObjectType[S], encoder: Encoder[F, Object[S], A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.encodeO[F, S, A])
end EncoderObjectInstances
object EncoderObjectInstances extends EncoderObjectInstances
