package com.peknight.codec.instances

import com.peknight.codec.Decoder
import com.peknight.generic.priority.LowPriority

trait DecoderLowPriorityInstances:
  given lowPriorityDecoder[F[_], T, E, A](using instance: LowPriority[Decoder[F, T, E, A]]): Decoder[F, T, E, A] =
    instance.instance
  end lowPriorityDecoder
end DecoderLowPriorityInstances
