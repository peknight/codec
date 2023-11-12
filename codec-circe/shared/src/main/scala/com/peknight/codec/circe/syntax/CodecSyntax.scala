package com.peknight.codec.circe.syntax

import com.peknight.codec.id.{Codec, Decoder, Encoder}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.{ACursor, DecodingFailure, HCursor, Json, Codec as CirceCodec, Decoder as CirceDecoder, Encoder as CirceEncoder}

trait CodecSyntax:
  extension [A] (encoder: Encoder[Json, A])
    def asCirceEncoder: CirceEncoder[A] = encoder.encode(_)
  end extension

  extension [A] (decoder: Decoder[ACursor, DecodingFailure, A])
    def asCirceDecoder: CirceDecoder[A] =
      new CirceDecoder[A]:
        def apply(c: HCursor): Result[A] = decoder.decode(c)
        override def tryDecode(c: ACursor): Result[A] = decoder.decode(c)
        override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = decoder.decodeAccumulating(c)
        override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] = decoder.decodeAccumulating(c)
    end asCirceDecoder
  end extension

  extension [A] (codec: Codec[Json, ACursor, DecodingFailure, A])
    def asCirceCodec: CirceCodec[A] =
      new CirceCodec[A]:
        def apply(a: A): Json = codec.encode(a)
        def apply(c: HCursor): Result[A] = codec.decode(c)
        override def tryDecode(c: ACursor): Result[A] = codec.decode(c)
        override def decodeAccumulating(c: HCursor): AccumulatingResult[A] = codec.decodeAccumulating(c)
        override def tryDecodeAccumulating(c: ACursor): AccumulatingResult[A] = codec.decodeAccumulating(c)
    end asCirceCodec
  end extension
end CodecSyntax
object CodecSyntax extends CodecSyntax
