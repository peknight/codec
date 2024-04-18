package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType

trait EncoderValueInstances1:
  given stringEncodeBoolean[F[_]: Applicative]: Encoder[F, String, Boolean] = Encoder.encodeWithToString[F, Boolean]
  given encodeBooleanS[F[_]: Applicative, S: StringType]: Encoder[F, S, Boolean] =
    Encoder.encodeS[F, S, Boolean]

  given stringEncodeFloat[F[_]: Applicative]: Encoder[F, String, Float] = Encoder.encodeWithToString[F, Float]

  given encodeFloatS[F[_]: Applicative, S: StringType]: Encoder[F, S, Float] = Encoder.encodeS[F, S, Float]

  given stringEncodeDouble[F[_]: Applicative]: Encoder[F, String, Double] = Encoder.encodeWithToString[F, Double]

  given encodeDoubleS[F[_]: Applicative, S: StringType]: Encoder[F, S, Double] = Encoder.encodeS[F, S, Double]

  given stringEncodeByte[F[_]: Applicative]: Encoder[F, String, Byte] = Encoder.encodeWithToString[F, Byte]

  given encodeByteS[F[_]: Applicative, S: StringType]: Encoder[F, S, Byte] = Encoder.encodeS[F, S, Byte]

  given stringEncodeShort[F[_]: Applicative]: Encoder[F, String, Short] = Encoder.encodeWithToString[F, Short]

  given encodeShortS[F[_]: Applicative, S: StringType]: Encoder[F, S, Short] = Encoder.encodeS[F, S, Short]

  given stringEncodeInt[F[_]: Applicative]: Encoder[F, String, Int] = Encoder.encodeWithToString[F, Int]

  given encodeIntS[F[_]: Applicative, S: StringType]: Encoder[F, S, Int] = Encoder.encodeS[F, S, Int]

  given stringEncodeLong[F[_]: Applicative]: Encoder[F, String, Long] = Encoder.encodeWithToString[F, Long]

  given encodeLongS[F[_]: Applicative, S: StringType]: Encoder[F, S, Long] = Encoder.encodeS[F, S, Long]

  given stringEncodeBigInt[F[_]: Applicative]: Encoder[F, String, BigInt] = Encoder.encodeWithToString[F, BigInt]

  given encodeBigIntS[F[_]: Applicative, S: StringType]: Encoder[F, S, BigInt] = Encoder.encodeS[F, S, BigInt]

  given stringEncodeBigDecimal[F[_]: Applicative]: Encoder[F, String, BigDecimal] =
    Encoder.encodeWithToString[F, BigDecimal]

  given encodeBigDecimalS[F[_]: Applicative, S: StringType]: Encoder[F, S, BigDecimal] =
    Encoder.encodeS[F, S, BigDecimal]
end EncoderValueInstances1
