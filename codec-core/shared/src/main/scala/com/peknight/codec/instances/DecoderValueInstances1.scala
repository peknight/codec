package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}

trait DecoderValueInstances1:

  given booleanDecodeBoolean[F[_] : Applicative]: Decoder[F, Boolean, DecodingFailure, Boolean] =
    Decoder.instance[F, Boolean, DecodingFailure, Boolean](_.asRight.pure)

  given decodeStrictBoolean[F[_] : Applicative, S: BooleanType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.strictBooleanDecoder[F, S]

  given stringDecodeBoolean[F[_] : Applicative]: Decoder[F, String, DecodingFailure, Boolean] =
    Decoder.decodeWithOption[F, Boolean](Decoder.toBooleanOption)

  given decodeStringBoolean[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.stringDecoder[F, S, Boolean]

  given numberDecodeFloat[F[_]: Applicative]: Decoder[F, Number, DecodingFailure, Float] =
    Decoder.numberDecodeNumber[F, Float](_.toFloat)

  given decodeStrictFloat[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.strictNumberDecoder[F, S, Float]

  given stringDecodeFloat[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Float] =
    Decoder.stringDecodeNumber[F, Float](_.toFloat)

  given decodeStringFloat[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.stringDecoder[F, S, Float]

  given numberDecodeDouble[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, Double] =
    Decoder.numberDecodeNumber[F, Double](_.toDouble)

  given decodeStrictDouble[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.strictNumberDecoder[F, S, Double]

  given stringDecodeDouble[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Double] =
    Decoder.stringDecodeNumber[F, Double](_.toDouble)

  given decodeStringDouble[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.stringDecoder[F, S, Double]

  given numberDecodeByte[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, Byte] =
    Decoder.numberDecodeNumberOption[F, Byte](_.toByte)

  given decodeStrictByte[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.strictNumberDecoder[F, S, Byte]

  given stringDecodeByte[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Byte] =
    Decoder.stringDecodeNumberOption[F, Byte](_.toByte)

  given decodeStringByte[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.stringDecoder[F, S, Byte]

  given numberDecodeShort[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, Short] =
    Decoder.numberDecodeNumberOption[F, Short](_.toShort)

  given decodeStrictShort[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.strictNumberDecoder[F, S, Short]

  given stringDecodeShort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Short] =
    Decoder.stringDecodeNumberOption[F, Short](_.toShort)

  given decodeStringShort[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.stringDecoder[F, S, Short]

  given numberDecodeInt[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, Int] =
    Decoder.numberDecodeNumberOption[F, Int](_.toInt)

  given decodeStrictInt[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.strictNumberDecoder[F, S, Int]

  given stringDecodeInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Int] =
    Decoder.stringDecodeNumberOption[F, Int](_.toInt)

  given decodeStringInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.stringDecoder[F, S, Int]

  given numberDecodeLong[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, Long] =
    Decoder.numberDecodeNumberOption[F, Long](_.toLong)

  given decodeStrictLong[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.strictNumberDecoder[F, S, Long]

  given stringDecodeLong[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Long] =
    Decoder.stringDecodeNumberOption[F, Long](_.toLong)

  given decodeStringLong[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.stringDecoder[F, S, Long]

  given numberDecodeBigInt[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, BigInt] =
    Decoder.numberDecodeNumberOption[F, BigInt](_.toBigInt)

  given decodeStrictBigInt[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.strictNumberDecoder[F, S, BigInt]

  given stringDecodeBigInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigInt] =
    Decoder.stringDecodeNumberOption[F, BigInt](_.toBigInt)

  given decodeStringBigInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.stringDecoder[F, S, BigInt]

  given numberDecodeBigDecimal[F[_] : Applicative]: Decoder[F, Number, DecodingFailure, BigDecimal] =
    Decoder.numberDecodeNumberOption[F, BigDecimal](_.toBigDecimal)

  given decodeStrictBigDecimal[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.strictNumberDecoder[F, S, BigDecimal]

  given stringDecodeBigDecimal[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigDecimal] =
    Decoder.stringDecodeNumberOption[F, BigDecimal](_.toBigDecimal)

  given decodeStringBigDecimal[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.stringDecoder[F, S, BigDecimal]

end DecoderValueInstances1
