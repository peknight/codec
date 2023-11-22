package com.peknight.codec.instances

import com.peknight.codec.Decoder
import com.peknight.generic.priority.HighPriority

trait DecoderHighPriorityInstances extends DecoderMidPriorityInstances:
  given highPriorityDecoder[F[_], T, E, A](using highPriority: HighPriority[Decoder[F, T, E, A]]): Decoder[F, T, E, A] =
    highPriority.instance
  end highPriorityDecoder
end DecoderHighPriorityInstances
