package com.peknight.codec.circe.syntax

import cats.Id
import cats.data.ValidatedNel
import com.peknight.codec.id.{Decoder, Encoder, Codec}
import io.circe.{ACursor, DecodingFailure, Json, Decoder as CirceDecoder, Encoder as CirceEncoder, Codec as CirceCodec}

trait CirceSyntax:
  extension [A] (encoder: CirceEncoder[A])
    def asEncoder: Encoder[Json, A] = encoder.apply(_)
  end extension

  extension [A] (decoder: CirceDecoder[A])
    def asDecoder: Decoder[ACursor, DecodingFailure, A] =
      new Decoder[ACursor, DecodingFailure, A]:
        def decode(t: ACursor): Id[Either[DecodingFailure, A]] =
          decoder.tryDecode(t)
        def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] =
          decoder.tryDecodeAccumulating(t)
    end asDecoder
  end extension

  extension [A] (codec: CirceCodec[A])
    def asCodec: Codec[Json, ACursor, DecodingFailure, A] =
      new Codec[Json, ACursor, DecodingFailure, A]:
        def encode(a: A): Id[Json] = codec.apply(a)
        def decode(t: ACursor): Id[Either[DecodingFailure, A]] = codec.tryDecode(t)
        def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] = codec.tryDecodeAccumulating(t)
  end extension
end CirceSyntax
object CirceSyntax extends CirceSyntax

