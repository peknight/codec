package com.peknight.codec.instances

import com.peknight.codec.Encoder
import com.peknight.generic.priority.LowPriority

trait EncoderLowPriorityInstances:
  given lowPriorityEncoder[F[_], S, A](using instance: LowPriority[Encoder[F, S, A]]): Encoder[F, S, A] =
    instance.instance
  end lowPriorityEncoder
end EncoderLowPriorityInstances
