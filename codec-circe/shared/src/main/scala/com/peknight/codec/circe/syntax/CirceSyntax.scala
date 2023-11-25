package com.peknight.codec.circe.syntax

import cats.Id
import cats.data.ValidatedNel
import com.peknight.codec.id.{Codec, Decoder, Encoder}
import io.circe.{ACursor, DecodingFailure, Json}

trait CirceSyntax:
  extension [A] (encoder: io.circe.Encoder[A])
    def asEncoder: Encoder[Json, A] =
      encoder match
        case e: com.peknight.codec.circe.Encoder[A] => e.encoder
        case _ => encoder.apply(_)
  end extension

  extension [A] (decoder: io.circe.Decoder[A])
    def asDecoder: Decoder[ACursor, DecodingFailure, A] =
      new Decoder[ACursor, DecodingFailure, A]:
        def decode(t: ACursor): Id[Either[DecodingFailure, A]] =
          decoder.tryDecode(t)
        def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] =
          decoder.tryDecodeAccumulating(t)
    end asDecoder
  end extension

  extension [A] (codec: io.circe.Codec[A])
    def asCodec: Codec[Json, ACursor, DecodingFailure, A] =
      new Codec[Json, ACursor, DecodingFailure, A]:
        def encode(a: A): Id[Json] = codec.apply(a)
        def decode(t: ACursor): Id[Either[DecodingFailure, A]] = codec.tryDecode(t)
        def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] = codec.tryDecodeAccumulating(t)
  end extension
end CirceSyntax
object CirceSyntax extends CirceSyntax

