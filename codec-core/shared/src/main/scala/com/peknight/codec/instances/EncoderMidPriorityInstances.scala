package com.peknight.codec.instances

import com.peknight.codec.Encoder
import com.peknight.generic.priority.MidPriority

trait EncoderMidPriorityInstances extends EncoderLowPriorityInstances:
  given midPriorityEncoder[F[_], S, A](using midPriority: MidPriority[Encoder[F, S, A]]): Encoder[F, S, A] =
    midPriority.instance
  end midPriorityEncoder
end EncoderMidPriorityInstances
