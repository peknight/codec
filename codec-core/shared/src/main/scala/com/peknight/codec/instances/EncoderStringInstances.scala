package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import java.util.{Currency, UUID}
import scala.concurrent.duration.Duration

trait EncoderStringInstances:
  given stringEncodeString[F[_]: Applicative]: Encoder[F, String, String] =
    Encoder.instance[F, String, String](_.pure[F])

  given encodeString[F[_]: Applicative, S: StringType]: Encoder[F, S, String] =
    Encoder.stringEncoder[F, S, String](stringEncodeString[F])

  given stringEncodeBoolean[F[_]: Applicative]: Encoder[F, String, Boolean] = Encoder.toStringEncoder[F, Boolean]

  given encodeBoolean[F[_]: Applicative, S: StringType]: Encoder[F, S, Boolean] =
    Encoder.stringEncoder[F, S, Boolean](stringEncodeBoolean[F])

  given stringEncodeChar[F[_]: Applicative]: Encoder[F, String, Char] = Encoder.toStringEncoder[F, Char]

  given encodeChar[F[_]: Applicative, S: StringType]: Encoder[F, S, Char] =
    Encoder.stringEncoder[F, S, Char](stringEncodeChar[F])

  given stringEncodeFloat[F[_]: Applicative]: Encoder[F, String, Float] = Encoder.toStringEncoder[F, Float]

  given encodeFloat[F[_]: Applicative, S: StringType]: Encoder[F, S, Float] =
    Encoder.stringEncoder[F, S, Float](stringEncodeFloat[F])

  given stringEncodeDouble[F[_]: Applicative]: Encoder[F, String, Double] = Encoder.toStringEncoder[F, Double]

  given encodeDouble[F[_]: Applicative, S: StringType]: Encoder[F, S, Double] =
    Encoder.stringEncoder[F, S, Double](stringEncodeDouble[F])

  given stringEncodeByte[F[_]: Applicative]: Encoder[F, String, Byte] = Encoder.toStringEncoder[F, Byte]

  given encodeByte[F[_]: Applicative, S: StringType]: Encoder[F, S, Byte] =
    Encoder.stringEncoder[F, S, Byte](stringEncodeByte[F])

  given stringEncodeShort[F[_]: Applicative]: Encoder[F, String, Short] = Encoder.toStringEncoder[F, Short]

  given encodeShort[F[_]: Applicative, S: StringType]: Encoder[F, S, Short] =
    Encoder.stringEncoder[F, S, Short](stringEncodeShort[F])

  given stringEncodeInt[F[_]: Applicative]: Encoder[F, String, Int] = Encoder.toStringEncoder[F, Int]

  given encodeInt[F[_]: Applicative, S: StringType]: Encoder[F, S, Int] =
    Encoder.stringEncoder[F, S, Int](stringEncodeInt[F])

  given stringEncodeLong[F[_]: Applicative]: Encoder[F, String, Long] = Encoder.toStringEncoder[F, Long]

  given encodeLong[F[_]: Applicative, S: StringType]: Encoder[F, S, Long] =
    Encoder.stringEncoder[F, S, Long](stringEncodeLong[F])

  given stringEncodeBigInt[F[_]: Applicative]: Encoder[F, String, BigInt] = Encoder.toStringEncoder[F, BigInt]

  given encodeBigInt[F[_]: Applicative, S: StringType]: Encoder[F, S, BigInt] =
    Encoder.stringEncoder[F, S, BigInt](stringEncodeBigInt[F])

  given stringEncodeBigDecimal[F[_]: Applicative]: Encoder[F, String, BigDecimal] =
    Encoder.toStringEncoder[F, BigDecimal]

  given encodeBigDecimal[F[_]: Applicative, S: StringType]: Encoder[F, S, BigDecimal] =
    Encoder.stringEncoder[F, S, BigDecimal](stringEncodeBigDecimal[F])

  given stringEncodeUUID[F[_]: Applicative]: Encoder[F, String, UUID] = Encoder.toStringEncoder[F, UUID]

  given encodeUUID[F[_]: Applicative, S: StringType]: Encoder[F, S, UUID] =
    Encoder.stringEncoder[F, S, UUID](stringEncodeUUID[F])

  given stringEncodeURI[F[_]: Applicative]: Encoder[F, String, URI] = Encoder.toStringEncoder[F, URI]

  given encodeURI[F[_]: Applicative, S: StringType]: Encoder[F, S, URI] =
    Encoder.stringEncoder[F, S, URI](stringEncodeURI[F])

  given stringEncodeDuration[F[_]: Applicative]: Encoder[F, String, Duration] = Encoder.toStringEncoder[F, Duration]

  given encodeDuration[F[_]: Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.stringEncoder[F, S, Duration](stringEncodeDuration[F])

  given stringEncodeInstant[F[_]: Applicative]: Encoder[F, String, Instant] = Encoder.toStringEncoder[F, Instant]

  given encodeInstant[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] =
    Encoder.stringEncoder[F, S, Instant](stringEncodeInstant[F])

  given stringEncodePeriod[F[_]: Applicative]: Encoder[F, String, Period] = Encoder.toStringEncoder[F, Period]

  given encodePeriod[F[_]: Applicative, S: StringType]: Encoder[F, S, Period] =
    Encoder.stringEncoder[F, S, Period](stringEncodePeriod[F])

  given stringEncodeZoneId[F[_]: Applicative]: Encoder[F, String, ZoneId] with

    def encode(a: ZoneId): F[String] = a.getId.pure[F]
  end stringEncodeZoneId

  given encodeZoneId[F[_]: Applicative, S: StringType]: Encoder[F, S, ZoneId] =
    Encoder.stringEncoder[F, S, ZoneId](stringEncodeZoneId[F])

  given stringEncodeLocalDate[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDate]] =
    HighPriority(Encoder.toStringEncoder[F, LocalDate])

  given encodeLocalDate[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDate]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDate](stringEncodeLocalDate[F].instance))

  given stringEncodeLocalTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalTime](DateTimeFormatter.ISO_LOCAL_TIME))

  given encodeLocalTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalTime](stringEncodeLocalTime[F].instance))

  given stringEncodeLocalDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  given encodeLocalDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDateTime](stringEncodeLocalDateTime[F].instance))

  given stringEncodeMonthDay[F[_]: Applicative]: HighPriority[Encoder[F, String, MonthDay]] =
    HighPriority(Encoder.toStringEncoder[F, MonthDay])

  given encodeMonthDay[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, MonthDay]] =
    HighPriority(Encoder.stringEncoder[F, S, MonthDay](stringEncodeMonthDay[F].instance))

  given stringEncodeOffsetTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetTime](DateTimeFormatter.ISO_OFFSET_TIME))

  given encodeOffsetTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetTime](stringEncodeOffsetTime[F].instance))

  given stringEncodeOffsetDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetDateTime](DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  given encodeOffsetDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetDateTime](stringEncodeOffsetDateTime[F].instance))

  given stringEncodeYear[F[_]: Applicative]: HighPriority[Encoder[F, String, Year]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, Year](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .toFormatter()
    ))

  given encodeYear[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Year]] =
    HighPriority(Encoder.stringEncoder[F, S, Year](stringEncodeYear[F].instance))

  given stringEncodeYearMonth[F[_]: Applicative]: HighPriority[Encoder[F, String, YearMonth]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, YearMonth](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
    ))

  given encodeYearMonth[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, YearMonth]] =
    HighPriority(Encoder.stringEncoder[F, S, YearMonth](stringEncodeYearMonth[F].instance))

  given stringEncodeZonedDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, ZonedDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, ZonedDateTime](DateTimeFormatter.ISO_ZONED_DATE_TIME))

  given encodeZonedDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZonedDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, ZonedDateTime](stringEncodeZonedDateTime[F].instance))

  given stringEncodeZoneOffset[F[_]: Applicative]: HighPriority[Encoder[F, String, ZoneOffset]] =
    HighPriority(Encoder.toStringEncoder[F, ZoneOffset])

  given encodeZoneOffset[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZoneOffset]] =
    HighPriority(Encoder.stringEncoder[F, S, ZoneOffset](stringEncodeZoneOffset[F].instance))

  given stringEncodeCurrency[F[_]: Applicative]: HighPriority[Encoder[F, String, Currency]] =
    HighPriority(_.getCurrencyCode.pure[F])

  given encodeCurrency[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Currency]] =
    HighPriority(Encoder.stringEncoder[F, S, Currency](stringEncodeCurrency[F].instance))
end EncoderStringInstances
