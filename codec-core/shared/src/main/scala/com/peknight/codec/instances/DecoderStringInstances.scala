package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{DecodingFailure, WrongClassTag}
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.util.{Currency, UUID}
import scala.reflect.ClassTag

trait DecoderStringInstances extends DecoderStringInstances1:
  given stringDecodeString[F[_]: Applicative]: Decoder[F, String, DecodingFailure, String] =
    Decoder.instance[F, String, DecodingFailure, String](_.asRight.pure)

  given decodeString[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, String] =
    Decoder.stringDecoder[F, S, String](stringDecodeString[F])

  given stringDecodeBoolean[F[_] : Applicative]: Decoder[F, String, DecodingFailure, Boolean] =
    Decoder.instance[F, String, DecodingFailure, Boolean] { t =>
      Decoder.toBooleanOption(t) match
        case Some(b) => b.asRight.pure
        case None => WrongClassTag[Boolean].value(t).asLeft.pure
    }

  given decodeBoolean[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.stringDecoder[F, S, Boolean](stringDecodeBoolean[F])

  given stringDecodeChar[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Char] =
    Decoder.instance[F, String, DecodingFailure, Char](t =>
      if t.length == 1 then t.head.asRight.pure else WrongClassTag[Char].value(t).asLeft.pure
    )

  given decodeChar[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Char] =
    Decoder.stringDecoder[F, S, Char](stringDecodeChar[F])

  given stringDecodeFloat[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Float] =
    Decoder.decodeNumber[F, Float](_.toFloat)

  given decodeFloat[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.stringDecoder[F, S, Float](stringDecodeFloat[F])

  given stringDecodeDouble[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Double] =
    Decoder.decodeNumber[F, Double](_.toDouble)

  given decodeDouble[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.stringDecoder[F, S, Double](stringDecodeDouble[F])

  given stringDecodeByte[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Byte] =
    Decoder.decodeNumber[F, Byte](_.toByte)

  given decodeByte[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.stringDecoder[F, S, Byte](stringDecodeByte[F])

  given stringDecodeShort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Short] =
    Decoder.decodeNumber[F, Short](_.toShort)

  given decodeShort[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.stringDecoder[F, S, Short](stringDecodeShort[F])

  given stringDecodeInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Int] =
    Decoder.decodeNumber[F, Int](_.toInt)

  given decodeInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.stringDecoder[F, S, Int](stringDecodeInt[F])

  given stringDecodeLong[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Long] =
    Decoder.decodeNumber[F, Long](_.toLong)

  given decodeLong[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.stringDecoder[F, S, Long](stringDecodeLong[F])

  given stringDecodeBigInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigInt] =
    Decoder.decodeNumber[F, BigInt](_.toBigInt)

  given decodeBigInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.stringDecoder[F, S, BigInt](stringDecodeBigInt[F])

  given stringDecodeBigDecimal[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigDecimal] =
    Decoder.decodeNumber[F, BigDecimal](identity)

  given decodeBigDecimal[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.stringDecoder[F, S, BigDecimal](stringDecodeBigDecimal[F])

  given stringDecodeUUID[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, UUID]] =
    HighPriority(Decoder.decodeWithTry[F, UUID](UUID.fromString))

  given decodeUUID[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, UUID]] =
    HighPriority(Decoder.stringDecoder[F, S, UUID](stringDecodeUUID[F].instance))

  given stringDecodeURI[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, URI]] =
    HighPriority(Decoder.decodeWithTry[F, URI](t => new URI(t)))

  given decodeURI[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, URI]] =
    HighPriority(Decoder.stringDecoder[F, S, URI](stringDecodeURI[F].instance))

  given stringDecodeDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Duration]] =
    HighPriority(Decoder.decodeWithTry[F, Duration](Duration.parse))

  given decodeDuration[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, Duration]] =
    HighPriority(Decoder.stringDecoder[F, S, Duration](stringDecodeDuration[F].instance))

  given stringDecodeInstant[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Instant]] =
    HighPriority(Decoder.decodeWithTry[F, Instant](Instant.parse))

  given decodeInstant[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Instant]] =
    HighPriority(Decoder.stringDecoder[F, S, Instant](stringDecodeInstant[F].instance))

  given stringDecodePeriod[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Period]] =
    HighPriority(Decoder.decodeWithTry[F, Period](Period.parse))

  given decodePeriod[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Period]] =
    HighPriority(Decoder.stringDecoder[F, S, Period](stringDecodePeriod[F].instance))

  given stringDecodeZoneId[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneId]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneId](ZoneId.of))

  given decodeZoneId[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneId]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneId](stringDecodeZoneId[F].instance))

  given stringDecodeLocalDate[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDate]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDate](LocalDate.parse))

  given decodeLocalDate[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDate]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDate](stringDecodeLocalDate[F].instance))

  given stringDecodeLocalTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalTime](LocalTime.parse))

  given decodeLocalTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalTime](stringDecodeLocalTime[F].instance))

  given stringDecodeLocalDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDateTime](LocalDateTime.parse))

  given decodeLocalDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDateTime](stringDecodeLocalDateTime[F].instance))

  given stringDecodeMonthDay[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, MonthDay]] =
    HighPriority(Decoder.decodeWithTry[F, MonthDay](MonthDay.parse))

  given decodeMonthDay[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, MonthDay]] =
    HighPriority(Decoder.stringDecoder[F, S, MonthDay](stringDecodeMonthDay[F].instance))

  given stringDecodeOffsetTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetTime](OffsetTime.parse))

  given decodeOffsetTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetTime](stringDecodeOffsetTime[F].instance))

  given stringDecodeOffsetDateTime[F[_]: Applicative]
  : HighPriority[Decoder[F, String, DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetDateTime](OffsetDateTime.parse))

  given decodeOffsetDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetDateTime](stringDecodeOffsetDateTime[F].instance))

  given stringDecodeYear[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Year]] =
    HighPriority(Decoder.decodeWithTry[F, Year](Year.parse))

  given decodeYear[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Year]] =
    HighPriority(Decoder.stringDecoder[F, S, Year](stringDecodeYear[F].instance))

  given stringDecodeYearMonth[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, YearMonth]] =
    HighPriority(Decoder.decodeWithTry[F, YearMonth](YearMonth.parse))

  given decodeYearMonth[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, YearMonth]] =
    HighPriority(Decoder.stringDecoder[F, S, YearMonth](stringDecodeYearMonth[F].instance))

  given stringDecodeZonedDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, ZonedDateTime](ZonedDateTime.parse))

  given decodeZonedDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, ZonedDateTime](stringDecodeZonedDateTime[F].instance))

  given stringDecodeZoneOffset[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneOffset](ZoneOffset.of))

  given decodeZoneOffset[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneOffset](stringDecodeZoneOffset[F].instance))

  given stringDecodeCurrency[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Currency]] =
    HighPriority(Decoder.decodeWithTry[F, Currency](Currency.getInstance))

  given decodeCurrency[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, Currency]] =
    HighPriority(Decoder.stringDecoder[F, S, Currency](stringDecodeCurrency[F].instance))
end DecoderStringInstances
