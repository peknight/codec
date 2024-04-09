package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType

trait EncoderValueInstances1:
  given stringEncodeBoolean[F[_]: Applicative]: Encoder[F, String, Boolean] = Encoder.toStringEncoder[F, Boolean]
  given encodeStringBoolean[F[_]: Applicative, S: StringType]: Encoder[F, S, Boolean] =
    Encoder.stringEncoder[F, S, Boolean]

  given stringEncodeFloat[F[_]: Applicative]: Encoder[F, String, Float] = Encoder.toStringEncoder[F, Float]

  given encodeStringFloat[F[_]: Applicative, S: StringType]: Encoder[F, S, Float] = Encoder.stringEncoder[F, S, Float]

  given stringEncodeDouble[F[_]: Applicative]: Encoder[F, String, Double] = Encoder.toStringEncoder[F, Double]

  given encodeStringDouble[F[_]: Applicative, S: StringType]: Encoder[F, S, Double] = Encoder.stringEncoder[F, S, Double]

  given stringEncodeByte[F[_]: Applicative]: Encoder[F, String, Byte] = Encoder.toStringEncoder[F, Byte]

  given encodeStringByte[F[_]: Applicative, S: StringType]: Encoder[F, S, Byte] = Encoder.stringEncoder[F, S, Byte]

  given stringEncodeShort[F[_]: Applicative]: Encoder[F, String, Short] = Encoder.toStringEncoder[F, Short]

  given encodeStringShort[F[_]: Applicative, S: StringType]: Encoder[F, S, Short] = Encoder.stringEncoder[F, S, Short]

  given stringEncodeInt[F[_]: Applicative]: Encoder[F, String, Int] = Encoder.toStringEncoder[F, Int]

  given encodeStringInt[F[_]: Applicative, S: StringType]: Encoder[F, S, Int] = Encoder.stringEncoder[F, S, Int]

  given stringEncodeLong[F[_]: Applicative]: Encoder[F, String, Long] = Encoder.toStringEncoder[F, Long]

  given encodeStringLong[F[_]: Applicative, S: StringType]: Encoder[F, S, Long] = Encoder.stringEncoder[F, S, Long]

  given stringEncodeBigInt[F[_]: Applicative]: Encoder[F, String, BigInt] = Encoder.toStringEncoder[F, BigInt]

  given encodeStringBigInt[F[_]: Applicative, S: StringType]: Encoder[F, S, BigInt] = Encoder.stringEncoder[F, S, BigInt]

  given stringEncodeBigDecimal[F[_]: Applicative]: Encoder[F, String, BigDecimal] =
    Encoder.toStringEncoder[F, BigDecimal]

  given encodeStringBigDecimal[F[_]: Applicative, S: StringType]: Encoder[F, S, BigDecimal] =
    Encoder.stringEncoder[F, S, BigDecimal]
end EncoderValueInstances1
