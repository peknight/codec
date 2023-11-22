package com.peknight.codec.instances

import com.peknight.codec.Decoder
import com.peknight.generic.priority.MidPriority

trait DecoderMidPriorityInstances extends DecoderLowPriorityInstances:
  given midPriorityDecoder[F[_], T, E, A](using midPriority: MidPriority[Decoder[F, T, E, A]]): Decoder[F, T, E, A] =
    midPriority.instance
  end midPriorityDecoder
end DecoderMidPriorityInstances
