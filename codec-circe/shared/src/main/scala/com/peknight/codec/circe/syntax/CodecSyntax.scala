package com.peknight.codec.circe.syntax

import com.peknight.codec.circe.{Codec, Decoder, Encoder}
import io.circe.{ACursor, DecodingFailure, Json}

trait CodecSyntax:
  extension [A] (encoder: com.peknight.codec.id.Encoder[Json, A])
    def asCirceEncoder: Encoder[A] = Encoder(encoder)
  end extension

  extension [A] (decoder: com.peknight.codec.id.Decoder[ACursor, DecodingFailure, A])
    def asCirceDecoder: Decoder[A] = Decoder(decoder)
  end extension

  extension [A] (codec: com.peknight.codec.id.Codec[Json, ACursor, DecodingFailure, A])
    def asCirceCodec: Codec[A] = Codec(codec)
  end extension
end CodecSyntax
object CodecSyntax extends CodecSyntax
