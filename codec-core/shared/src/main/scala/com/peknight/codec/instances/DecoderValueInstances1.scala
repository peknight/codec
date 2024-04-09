package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType

trait DecoderValueInstances1:

  given stringDecodeBoolean[F[_] : Applicative]: Decoder[F, String, DecodingFailure, Boolean] =
    Decoder.decodeWithOption[F, Boolean](Decoder.toBooleanOption)

  given decodeStringBoolean[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.stringDecoder[F, S, Boolean]

  given stringDecodeFloat[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Float] =
    Decoder.decodeNumber[F, Float](_.toFloat)

  given decodeStringFloat[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.stringDecoder[F, S, Float]

  given stringDecodeDouble[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Double] =
    Decoder.decodeNumber[F, Double](_.toDouble)

  given decodeStringDouble[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.stringDecoder[F, S, Double]

  given stringDecodeByte[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Byte] =
    Decoder.decodeNumber[F, Byte](_.toByte)

  given decodeStringByte[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.stringDecoder[F, S, Byte]

  given stringDecodeShort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Short] =
    Decoder.decodeNumber[F, Short](_.toShort)

  given decodeStringShort[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.stringDecoder[F, S, Short]

  given stringDecodeInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Int] =
    Decoder.decodeNumber[F, Int](_.toInt)

  given decodeStringInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.stringDecoder[F, S, Int]

  given stringDecodeLong[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Long] =
    Decoder.decodeNumber[F, Long](_.toLong)

  given decodeStringLong[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.stringDecoder[F, S, Long]

  given stringDecodeBigInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigInt] =
    Decoder.decodeNumber[F, BigInt](_.toBigInt)

  given decodeStringBigInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.stringDecoder[F, S, BigInt]

  given stringDecodeBigDecimal[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigDecimal] =
    Decoder.decodeNumber[F, BigDecimal](identity)

  given decodeStringBigDecimal[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.stringDecoder[F, S, BigDecimal]

end DecoderValueInstances1
