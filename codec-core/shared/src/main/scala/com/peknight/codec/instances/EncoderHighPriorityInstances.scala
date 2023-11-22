package com.peknight.codec.instances

import com.peknight.codec.Encoder
import com.peknight.generic.priority.HighPriority

trait EncoderHighPriorityInstances extends EncoderMidPriorityInstances:
  given highPriorityEncoder[F[_], S, A](using highPriority: HighPriority[Encoder[F, S, A]]): Encoder[F, S, A] =
    highPriority.instance
  end highPriorityEncoder
end EncoderHighPriorityInstances
