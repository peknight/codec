package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asDecoder
import com.peknight.codec.id.Decoder
import com.peknight.generic.priority.LowPriority
import io.circe.{ACursor, DecodingFailure, Decoder as CirceDecoder}

trait DecoderInstances:
  given circeDecoder[A](using decoder: CirceDecoder[A]): LowPriority[Decoder[ACursor, DecodingFailure, A]] =
    LowPriority(decoder.asDecoder)
end DecoderInstances
object DecoderInstances extends DecoderInstances
