package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asDecoder
import com.peknight.codec.id.Decoder
import com.peknight.generic.priority.LowPriority
import io.circe.{ACursor, DecodingFailure}

trait DecoderCirceInstances:
  given circeDecoder[A](using decoder: io.circe.Decoder[A]): LowPriority[Decoder[ACursor, DecodingFailure, A]] =
    LowPriority(decoder.asDecoder)
end DecoderCirceInstances

object DecoderCirceInstances extends DecoderCirceInstances
