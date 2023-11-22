package com.peknight.codec.instances

import com.peknight.codec.Decoder
import com.peknight.generic.priority.LowPriority

trait DecoderLowPriorityInstances:
  given lowPriorityDecoder[F[_], T, E, A](using lowPriority: LowPriority[Decoder[F, T, E, A]]): Decoder[F, T, E, A] =
    lowPriority.instance
  end lowPriorityDecoder
end DecoderLowPriorityInstances
