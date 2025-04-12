package com.peknight.codec.instances

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import java.time.{Duration as JDuration, *}
import java.util.{Currency, UUID}
import scala.concurrent.duration.{Duration, FiniteDuration}

trait EncoderValueInstances extends EncoderValueInstances1:
  given stringEncodeString[F[_]: Applicative]: Encoder[F, String, String] = Encoder.identity[F, String]

  given encodeStringS[F[_]: Applicative, S: StringType]: Encoder[F, S, String] = Encoder.encodeS[F, S, String]

  given booleanEncodeBoolean[F[_]: Applicative]: Encoder[F, Boolean, Boolean] = Encoder.identity[F, Boolean]

  given encodeBooleanB[F[_]: Applicative, S: BooleanType]: Encoder[F, S, Boolean] = Encoder.encodeB[F, S]

  given stringEncodeChar[F[_]: Applicative]: Encoder[F, String, Char] = Encoder.encodeWithToString[F, Char]

  given encodeCharS[F[_]: Applicative, S: StringType]: Encoder[F, S, Char] = Encoder.encodeS[F, S, Char]

  given numberEncodeFloat[F[_]: Applicative]: Encoder[F, Number, Float] =
    Encoder.map[F, Number, Float](Number.fromFloat)

  given encodeFloatN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Float] = Encoder.encodeN[F, S, Float]

  given numberEncodeDouble[F[_]: Applicative]: Encoder[F, Number, Double] =
    Encoder.map[F, Number, Double](Number.fromDouble)

  given encodeDoubleN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Double] =
    Encoder.encodeN[F, S, Double]

  given numberEncodeByte[F[_]: Applicative]: Encoder[F, Number, Byte] =
    Encoder.map[F, Number, Byte](Number.fromByte)

  given encodeByteN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Byte] = Encoder.encodeN[F, S, Byte]

  given numberEncodeShort[F[_]: Applicative]: Encoder[F, Number, Short] =
    Encoder.map[F, Number, Short](Number.fromShort)

  given encodeShortN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Short] = Encoder.encodeN[F, S, Short]

  given numberEncodeInt[F[_]: Applicative]: Encoder[F, Number, Int] =
    Encoder.map[F, Number, Int](Number.fromInt)

  given encodeIntN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Int] = Encoder.encodeN[F, S, Int]

  given numberEncodeLong[F[_]: Applicative]: Encoder[F, Number, Long] =
    Encoder.map[F, Number, Long](Number.fromLong)

  given encodeLongN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Long] = Encoder.encodeN[F, S, Long]

  given numberEncodeBigInt[F[_]: Applicative]: Encoder[F, Number, BigInt] =
    Encoder.map[F, Number, BigInt](Number.fromBigInt)

  given encodeBigIntN[F[_]: Applicative, S: NumberType]: Encoder[F, S, BigInt] =
    Encoder.encodeN[F, S, BigInt]

  given numberEncodeBigDecimal[F[_]: Applicative]: Encoder[F, Number, BigDecimal] =
    Encoder.map[F, Number, BigDecimal](Number.fromBigDecimal)

  given encodeBigDecimalN[F[_]: Applicative, S: NumberType]: Encoder[F, S, BigDecimal] =
    Encoder.encodeN[F, S, BigDecimal]

  given stringEncodeUUID[F[_]: Applicative]: Encoder[F, String, UUID] = Encoder.encodeWithToString[F, UUID]

  given encodeUUIDS[F[_]: Applicative, S: StringType]: Encoder[F, S, UUID] = Encoder.encodeS[F, S, UUID]

  given stringEncodeURI[F[_]: Applicative]: Encoder[F, String, URI] = Encoder.encodeWithToString[F, URI]

  given encodeURIS[F[_]: Applicative, S: StringType]: Encoder[F, S, URI] = Encoder.encodeS[F, S, URI]

  given stringEncodeJavaDuration[F[_]: Applicative]: Encoder[F, String, JDuration] =
    Encoder.encodeWithToString[F, JDuration]

  given encodeJavaDurationS[F[_]: Applicative, S: StringType]: Encoder[F, S, JDuration] =
    Encoder.encodeS[F, S, JDuration]

  given stringEncodeDuration[F[_] : Applicative]: Encoder[F, String, Duration] = Encoder.encodeWithToString[F, Duration]

  given encodeDurationS[F[_] : Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.encodeS[F, S, Duration]

  given stringEncodeFiniteDuration[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.encodeWithToString[F, FiniteDuration]

  given encodeFiniteDurationS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration]

  given stringEncodeInstant[F[_]: Applicative]: Encoder[F, String, Instant] = Encoder.encodeWithToString[F, Instant]

  given encodeInstantS[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] = Encoder.encodeS[F, S, Instant]

  given stringEncodePeriod[F[_]: Applicative]: Encoder[F, String, Period] = Encoder.encodeWithToString[F, Period]

  given encodePeriodS[F[_]: Applicative, S: StringType]: Encoder[F, S, Period] = Encoder.encodeS[F, S, Period]

  given stringEncodeZoneId[F[_]: Applicative]: Encoder[F, String, ZoneId] = Encoder.map(_.getId)

  given encodeZoneIdS[F[_]: Applicative, S: StringType]: Encoder[F, S, ZoneId] = Encoder.encodeS[F, S, ZoneId]

  given stringEncodeLocalDate[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDate]] =
    HighPriority(Encoder.encodeWithToString[F, LocalDate])

  given encodeLocalDateS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDate]] =
    HighPriority(Encoder.encodeS[F, S, LocalDate])

  given stringEncodeLocalTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalTime](DateTimeFormatter.ISO_LOCAL_TIME))

  given encodeLocalTimeS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalTime]] =
    HighPriority(Encoder.encodeS[F, S, LocalTime])

  given stringEncodeLocalDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  given encodeLocalDateTimeS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDateTime]] =
    HighPriority(Encoder.encodeS[F, S, LocalDateTime])

  given stringEncodeMonthDay[F[_]: Applicative]: HighPriority[Encoder[F, String, MonthDay]] =
    HighPriority(Encoder.encodeWithToString[F, MonthDay])

  given encodeMonthDayS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, MonthDay]] =
    HighPriority(Encoder.encodeS[F, S, MonthDay])

  given stringEncodeOffsetTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetTime](DateTimeFormatter.ISO_OFFSET_TIME))

  given encodeOffsetTimeS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetTime]] =
    HighPriority(Encoder.encodeS[F, S, OffsetTime])

  given stringEncodeOffsetDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetDateTime](DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  given encodeOffsetDateTimeS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetDateTime]] =
    HighPriority(Encoder.encodeS[F, S, OffsetDateTime])

  given stringEncodeYear[F[_]: Applicative]: HighPriority[Encoder[F, String, Year]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, Year](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .toFormatter()
    ))

  given encodeYearS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Year]] =
    HighPriority(Encoder.encodeS[F, S, Year])

  given stringEncodeYearMonth[F[_]: Applicative]: HighPriority[Encoder[F, String, YearMonth]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, YearMonth](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
    ))

  given encodeYearMonthS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, YearMonth]] =
    HighPriority(Encoder.encodeS[F, S, YearMonth])

  given stringEncodeZonedDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, ZonedDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, ZonedDateTime](DateTimeFormatter.ISO_ZONED_DATE_TIME))

  given encodeZonedDateTimeS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZonedDateTime]] =
    HighPriority(Encoder.encodeS[F, S, ZonedDateTime])

  given stringEncodeZoneOffset[F[_]: Applicative]: HighPriority[Encoder[F, String, ZoneOffset]] =
    HighPriority(Encoder.encodeWithToString[F, ZoneOffset])

  given encodeZoneOffsetS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZoneOffset]] =
    HighPriority(Encoder.encodeS[F, S, ZoneOffset])

  given stringEncodeCurrency[F[_]: Applicative]: HighPriority[Encoder[F, String, Currency]] =
    HighPriority(Encoder.map(_.getCurrencyCode))

  given encodeCurrencyS[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Currency]] =
    HighPriority(Encoder.encodeS[F, S, Currency])
end EncoderValueInstances
