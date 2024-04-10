package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import java.util.{Currency, UUID}
import scala.concurrent.duration.Duration

trait EncoderValueInstances extends EncoderValueInstances1:
  given stringEncodeString[F[_]: Applicative]: Encoder[F, String, String] =
    Encoder.instance[F, String, String](_.pure[F])

  given encodeString[F[_]: Applicative, S: StringType]: Encoder[F, S, String] = Encoder.stringEncoder[F, S, String]

  given booleanEncodeBoolean[F[_]: Applicative]: Encoder[F, Boolean, Boolean] =
    Encoder.instance[F, Boolean, Boolean](_.pure[F])

  given encodeBooleanBoolean[F[_]: Applicative, S: BooleanType]: Encoder[F, S, Boolean] =
    Encoder.booleanEncoder[F, S]

  given stringEncodeChar[F[_]: Applicative]: Encoder[F, String, Char] = Encoder.toStringEncoder[F, Char]

  given encodeChar[F[_]: Applicative, S: StringType]: Encoder[F, S, Char] = Encoder.stringEncoder[F, S, Char]

  given numberEncodeFloat[F[_]: Applicative]: Encoder[F, Number, Float] =
    Encoder.instance[F, Number, Float](Number.fromFloat(_).pure[F])

  given encodeNumberFloat[F[_]: Applicative, S: NumberType]: Encoder[F, S, Float] = Encoder.numberEncoder[F, S, Float]

  given numberEncodeDouble[F[_]: Applicative]: Encoder[F, Number, Double] =
    Encoder.instance[F, Number, Double](Number.fromDouble(_).pure[F])

  given encodeNumberDouble[F[_]: Applicative, S: NumberType]: Encoder[F, S, Double] = Encoder.numberEncoder[F, S, Double]

  given numberEncodeByte[F[_]: Applicative]: Encoder[F, Number, Byte] =
    Encoder.instance[F, Number, Byte](Number.fromByte(_).pure[F])

  given encodeNumberByte[F[_]: Applicative, S: NumberType]: Encoder[F, S, Byte] = Encoder.numberEncoder[F, S, Byte]

  given numberEncodeShort[F[_]: Applicative]: Encoder[F, Number, Short] =
    Encoder.instance[F, Number, Short](Number.fromShort(_).pure[F])

  given encodeNumberShort[F[_]: Applicative, S: NumberType]: Encoder[F, S, Short] = Encoder.numberEncoder[F, S, Short]

  given numberEncodeInt[F[_]: Applicative]: Encoder[F, Number, Int] =
    Encoder.instance[F, Number, Int](Number.fromInt(_).pure[F])

  given encodeNumberInt[F[_]: Applicative, S: NumberType]: Encoder[F, S, Int] = Encoder.numberEncoder[F, S, Int]

  given numberEncodeLong[F[_]: Applicative]: Encoder[F, Number, Long] =
    Encoder.instance[F, Number, Long](Number.fromLong(_).pure[F])

  given encodeNumberLong[F[_]: Applicative, S: NumberType]: Encoder[F, S, Long] = Encoder.numberEncoder[F, S, Long]

  given numberEncodeBigInt[F[_]: Applicative]: Encoder[F, Number, BigInt] =
    Encoder.instance[F, Number, BigInt](Number.fromBigInt(_).pure[F])

  given encodeNumberBigInt[F[_]: Applicative, S: NumberType]: Encoder[F, S, BigInt] =
    Encoder.numberEncoder[F, S, BigInt]

  given numberEncodeBigDecimal[F[_]: Applicative]: Encoder[F, Number, BigDecimal] =
    Encoder.instance[F, Number, BigDecimal](Number.fromBigDecimal(_).pure[F])

  given encodeNumberBigDecimal[F[_]: Applicative, S: NumberType]: Encoder[F, S, BigDecimal] =
    Encoder.numberEncoder[F, S, BigDecimal]

  given stringEncodeUUID[F[_]: Applicative]: Encoder[F, String, UUID] = Encoder.toStringEncoder[F, UUID]

  given encodeUUID[F[_]: Applicative, S: StringType]: Encoder[F, S, UUID] = Encoder.stringEncoder[F, S, UUID]

  given stringEncodeURI[F[_]: Applicative]: Encoder[F, String, URI] = Encoder.toStringEncoder[F, URI]

  given encodeURI[F[_]: Applicative, S: StringType]: Encoder[F, S, URI] = Encoder.stringEncoder[F, S, URI]

  given stringEncodeDuration[F[_]: Applicative]: Encoder[F, String, Duration] = Encoder.toStringEncoder[F, Duration]

  given encodeDuration[F[_]: Applicative, S: StringType]: Encoder[F, S, Duration] = 
    Encoder.stringEncoder[F, S, Duration]

  given stringEncodeInstant[F[_]: Applicative]: Encoder[F, String, Instant] = Encoder.toStringEncoder[F, Instant]

  given encodeInstant[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] = Encoder.stringEncoder[F, S, Instant]

  given stringEncodePeriod[F[_]: Applicative]: Encoder[F, String, Period] = Encoder.toStringEncoder[F, Period]

  given encodePeriod[F[_]: Applicative, S: StringType]: Encoder[F, S, Period] = Encoder.stringEncoder[F, S, Period]

  given stringEncodeZoneId[F[_]: Applicative]: Encoder[F, String, ZoneId] with

    def encode(a: ZoneId): F[String] = a.getId.pure[F]
  end stringEncodeZoneId

  given encodeZoneId[F[_]: Applicative, S: StringType]: Encoder[F, S, ZoneId] = Encoder.stringEncoder[F, S, ZoneId]

  given stringEncodeLocalDate[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDate]] =
    HighPriority(Encoder.toStringEncoder[F, LocalDate])

  given encodeLocalDate[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDate]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDate])

  given stringEncodeLocalTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalTime](DateTimeFormatter.ISO_LOCAL_TIME))

  given encodeLocalTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalTime])

  given stringEncodeLocalDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  given encodeLocalDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDateTime])

  given stringEncodeMonthDay[F[_]: Applicative]: HighPriority[Encoder[F, String, MonthDay]] =
    HighPriority(Encoder.toStringEncoder[F, MonthDay])

  given encodeMonthDay[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, MonthDay]] =
    HighPriority(Encoder.stringEncoder[F, S, MonthDay])

  given stringEncodeOffsetTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetTime](DateTimeFormatter.ISO_OFFSET_TIME))

  given encodeOffsetTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetTime])

  given stringEncodeOffsetDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetDateTime](DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  given encodeOffsetDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetDateTime])

  given stringEncodeYear[F[_]: Applicative]: HighPriority[Encoder[F, String, Year]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, Year](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .toFormatter()
    ))

  given encodeYear[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Year]] =
    HighPriority(Encoder.stringEncoder[F, S, Year])

  given stringEncodeYearMonth[F[_]: Applicative]: HighPriority[Encoder[F, String, YearMonth]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, YearMonth](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
    ))

  given encodeYearMonth[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, YearMonth]] =
    HighPriority(Encoder.stringEncoder[F, S, YearMonth])

  given stringEncodeZonedDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, ZonedDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, ZonedDateTime](DateTimeFormatter.ISO_ZONED_DATE_TIME))

  given encodeZonedDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZonedDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, ZonedDateTime])

  given stringEncodeZoneOffset[F[_]: Applicative]: HighPriority[Encoder[F, String, ZoneOffset]] =
    HighPriority(Encoder.toStringEncoder[F, ZoneOffset])

  given encodeZoneOffset[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZoneOffset]] =
    HighPriority(Encoder.stringEncoder[F, S, ZoneOffset])

  given stringEncodeCurrency[F[_]: Applicative]: HighPriority[Encoder[F, String, Currency]] =
    HighPriority(_.getCurrencyCode.pure[F])

  given encodeCurrency[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Currency]] =
    HighPriority(Encoder.stringEncoder[F, S, Currency])
end EncoderValueInstances
