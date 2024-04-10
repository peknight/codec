package com.peknight.codec.instances.generic.encoder

import cats.{Applicative, Functor}
import com.peknight.codec.Encoder
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}
import com.peknight.generic.priority.MidPriority

trait EncoderValueInstances:
  given numberEncoder[F[_], S, A](using functor: Functor[F], numberType: NumberType[S], encoder: Encoder[F, Number, A])
  : MidPriority[Encoder[F, S, A]] =
    MidPriority(Encoder.numberEncoder[F, S, A])
end EncoderValueInstances
object EncoderValueInstances extends EncoderValueInstances
