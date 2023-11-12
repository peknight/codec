package com.peknight.codec.circe.instances

import com.peknight.codec.circe.syntax.circe.asDecoder
import com.peknight.codec.id.Decoder
import com.peknight.generic.Generic
import io.circe.{ACursor, DecodingFailure, Decoder as CirceDecoder}

trait DecoderInstances:
  given stringOptionDecoder: Decoder[ACursor, DecodingFailure, Option[String]] =
    CirceDecoder.decodeOption[String].asDecoder

  given genericDecoderInstances[A](using instances: => Generic.Instances[CirceDecoder, A])
  : Generic.Instances[[X] =>> Decoder[ACursor, DecodingFailure, X], A] =
    instances.mapK[[X] =>> Decoder[ACursor, DecodingFailure, X]](
      [X] => (decoder: CirceDecoder[X]) => decoder.asDecoder
    )
end DecoderInstances
object DecoderInstances extends DecoderInstances
