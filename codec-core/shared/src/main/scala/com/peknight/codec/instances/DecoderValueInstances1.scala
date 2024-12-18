package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}

trait DecoderValueInstances1:
  given booleanDecodeBoolean[F[_]: Applicative]: Decoder[F, Boolean, Boolean] =
    Decoder.identity[F, Boolean]

  given decodeBooleanB[F[_]: Applicative, S: BooleanType]: Decoder[F, Cursor[S], Boolean] =
    Decoder.decodeB[F, S]

  given stringDecodeBoolean[F[_]: Applicative]: Decoder[F, String, Boolean] =
    Decoder.mapOption[F, String, Boolean](Decoder.toBooleanOption)

  given decodeBooleanS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Boolean] =
    Decoder.decodeS[F, S, Boolean]

  given numberDecodeFloat[F[_]: Applicative]: Decoder[F, Number, Float] =
    Decoder.numberDecodeNumber[F, Float](_.toFloat)

  given decodeFloatN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Float] =
    Decoder.decodeN[F, S, Float]

  given stringDecodeFloat[F[_]: Applicative]: Decoder[F, String, Float] =
    Decoder.stringDecodeWithNumberDecoder[F, Float]

  given decodeFloatS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Float] =
    Decoder.decodeS[F, S, Float]

  given numberDecodeDouble[F[_]: Applicative]: Decoder[F, Number, Double] =
    Decoder.numberDecodeNumber[F, Double](_.toDouble)

  given decodeDoubleN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Double] =
    Decoder.decodeN[F, S, Double]

  given stringDecodeDouble[F[_]: Applicative]: Decoder[F, String, Double] =
    Decoder.stringDecodeWithNumberDecoder[F, Double]

  given decodeDoubleS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Double] =
    Decoder.decodeS[F, S, Double]

  given numberDecodeByte[F[_]: Applicative]: Decoder[F, Number, Byte] =
    Decoder.numberDecodeNumberOption[F, Byte](_.toByte)

  given decodeByteN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Byte] =
    Decoder.decodeN[F, S, Byte]

  given stringDecodeByte[F[_]: Applicative]: Decoder[F, String, Byte] =
    Decoder.stringDecodeWithNumberDecoder[F, Byte]

  given decodeByteS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Byte] =
    Decoder.decodeS[F, S, Byte]

  given numberDecodeShort[F[_]: Applicative]: Decoder[F, Number, Short] =
    Decoder.numberDecodeNumberOption[F, Short](_.toShort)

  given decodeShortN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Short] =
    Decoder.decodeN[F, S, Short]

  given stringDecodeShort[F[_]: Applicative]: Decoder[F, String, Short] =
    Decoder.stringDecodeWithNumberDecoder[F, Short]

  given decodeShortS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Short] =
    Decoder.decodeS[F, S, Short]

  given numberDecodeInt[F[_]: Applicative]: Decoder[F, Number, Int] =
    Decoder.numberDecodeNumberOption[F, Int](_.toInt)

  given decodeIntN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Int] =
    Decoder.decodeN[F, S, Int]

  given stringDecodeInt[F[_]: Applicative]: Decoder[F, String, Int] =
    Decoder.stringDecodeWithNumberDecoder[F, Int]

  given decodeIntS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Int] =
    Decoder.decodeS[F, S, Int]

  given numberDecodeLong[F[_]: Applicative]: Decoder[F, Number, Long] =
    Decoder.numberDecodeNumberOption[F, Long](_.toLong)

  given decodeLongN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Long] =
    Decoder.decodeN[F, S, Long]

  given stringDecodeLong[F[_]: Applicative]: Decoder[F, String, Long] =
    Decoder.stringDecodeWithNumberDecoder[F, Long]

  given decodeLongS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Long] =
    Decoder.decodeS[F, S, Long]

  given numberDecodeBigInt[F[_]: Applicative]: Decoder[F, Number, BigInt] =
    Decoder.numberDecodeNumberOption[F, BigInt](_.toBigInt)

  given decodeBigIntN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], BigInt] =
    Decoder.decodeN[F, S, BigInt]

  given stringDecodeBigInt[F[_]: Applicative]: Decoder[F, String, BigInt] =
    Decoder.stringDecodeWithNumberDecoder[F, BigInt]

  given decodeBigIntS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], BigInt] =
    Decoder.decodeS[F, S, BigInt]

  given numberDecodeBigDecimal[F[_]: Applicative]: Decoder[F, Number, BigDecimal] =
    Decoder.numberDecodeNumberOption[F, BigDecimal](_.toBigDecimal)

  given decodeBigDecimalN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], BigDecimal] =
    Decoder.decodeN[F, S, BigDecimal]

  given stringDecodeBigDecimal[F[_]: Applicative]: Decoder[F, String, BigDecimal] =
    Decoder.stringDecodeWithNumberDecoder[F, BigDecimal]

  given decodeBigDecimalS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], BigDecimal] =
    Decoder.decodeS[F, S, BigDecimal]
end DecoderValueInstances1
