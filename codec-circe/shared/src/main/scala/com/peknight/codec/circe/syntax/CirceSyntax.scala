package com.peknight.codec.circe.syntax

import cats.Id
import cats.data.ValidatedNel
import com.peknight.codec
import com.peknight.codec.derivation.{SumDecoder, SumEncoder}
import com.peknight.codec.id.{Codec, Decoder, Encoder}
import io.circe.derivation.{ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, SumOrProductOps}
import io.circe.{ACursor, DecodingFailure, Json}

trait CirceSyntax:
  extension [A] (encoder: io.circe.Encoder[A])
    def asEncoder: Encoder[Json, A] =
      encoder match
        case e: com.peknight.codec.circe.Encoder[A] => e.encoder
        case e: ConfiguredEncoder[A] if SumOrProductOps.isSum(e) =>
          new SumEncoder[Id, Json, A]:
            def encode(a: A): Id[Json] = encoder(a)
        case e => e(_)
  end extension

  extension [A] (decoder: io.circe.Decoder[A])
    def asDecoder: Decoder[ACursor, DecodingFailure, A] =
      decoder match
        case d: com.peknight.codec.circe.Decoder[A] => d.decoder
        case d: ConfiguredDecoder[A] if SumOrProductOps.isSum(d) =>
          new SumDecoder[Id, ACursor, DecodingFailure, A]:
            def decoders: Map[String, Decoder[ACursor, DecodingFailure, _]] =
              d.constructorNames.zip(d.elemDecoders).map((key, dd) => (key, dd.asDecoder)).toMap
            def decode(t: ACursor): Id[Either[DecodingFailure, A]] =
              decoder.tryDecode(t)
            def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] =
              decoder.tryDecodeAccumulating(t)
        case d =>
          new Decoder[ACursor, DecodingFailure, A]:
            def decode(t: ACursor): Id[Either[DecodingFailure, A]] =
              d.tryDecode(t)
            def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] =
              d.tryDecodeAccumulating(t)
    end asDecoder
  end extension

  extension [A] (codec: io.circe.Codec[A])
    def asCodec: Codec[Json, ACursor, DecodingFailure, A] =
      codec match
        case c: com.peknight.codec.circe.Codec[A] => c.codec
        case c: ConfiguredCodec[A] if SumOrProductOps.isSum(c) =>
          new Codec[Json, ACursor, DecodingFailure, A] with SumEncoder[Id, Json, A]
            with SumDecoder[Id, ACursor, DecodingFailure, A]:
            def decoders: Map[String, Decoder[ACursor, DecodingFailure, _]] =
              c.constructorNames.zip(c.elemDecoders).map((key, dd) => (key, dd.asDecoder)).toMap
            def encode(a: A): Id[Json] = c.apply(a)
            def decode(t: ACursor): Id[Either[DecodingFailure, A]] = c.tryDecode(t)
            def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] = c.tryDecodeAccumulating(t)
        case c =>
          new Codec[Json, ACursor, DecodingFailure, A]:
            def encode(a: A): Id[Json] = c.apply(a)
            def decode(t: ACursor): Id[Either[DecodingFailure, A]] = c.tryDecode(t)
            def decodeAccumulating(t: ACursor): Id[ValidatedNel[DecodingFailure, A]] = c.tryDecodeAccumulating(t)
  end extension
end CirceSyntax
object CirceSyntax extends CirceSyntax

