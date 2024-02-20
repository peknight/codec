package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import java.util.{Currency, UUID}
import scala.concurrent.duration.Duration

trait EncoderStringInstances1:
  given encodeStringBoolean[F[_]: Applicative]: Encoder[F, String, Boolean] = Encoder.toStringEncoder[F, Boolean]
  given encodeStringChar[F[_]: Applicative]: Encoder[F, String, Char] = Encoder.toStringEncoder[F, Char]
  given encodeStringFloat[F[_]: Applicative]: Encoder[F, String, Float] = Encoder.toStringEncoder[F, Float]
  given encodeStringDouble[F[_]: Applicative]: Encoder[F, String, Double] = Encoder.toStringEncoder[F, Double]
  given encodeStringByte[F[_]: Applicative]: Encoder[F, String, Byte] = Encoder.toStringEncoder[F, Byte]
  given encodeStringShort[F[_]: Applicative]: Encoder[F, String, Short] = Encoder.toStringEncoder[F, Short]
  given encodeStringInt[F[_]: Applicative]: Encoder[F, String, Int] = Encoder.toStringEncoder[F, Int]
  given encodeStringLong[F[_]: Applicative]: Encoder[F, String, Long] = Encoder.toStringEncoder[F, Long]
  given encodeStringBigInt[F[_]: Applicative]: Encoder[F, String, BigInt] = Encoder.toStringEncoder[F, BigInt]
  given encodeStringBigDecimal[F[_]: Applicative]: Encoder[F, String, BigDecimal] = Encoder.toStringEncoder[F, BigDecimal]
  given encodeStringUUID[F[_]: Applicative]: Encoder[F, String, UUID] = Encoder.toStringEncoder[F, UUID]
  given encodeStringURI[F[_]: Applicative]: Encoder[F, String, URI] = Encoder.toStringEncoder[F, URI]
  given encodeStringDuration[F[_]: Applicative]: Encoder[F, String, Duration] = Encoder.toStringEncoder[F, Duration]
  given encodeStringInstant[F[_]: Applicative]: Encoder[F, String, Instant] = Encoder.toStringEncoder[F, Instant]
  given encodeStringPeriod[F[_]: Applicative]: Encoder[F, String, Period] = Encoder.toStringEncoder[F, Period]
  given encodeStringZoneId[F[_]: Applicative]: Encoder[F, String, ZoneId] with
    def encode(a: ZoneId): F[String] = a.getId.pure[F]
  end encodeStringZoneId
  given encodeStringLocalDate[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDate]] =
    HighPriority(Encoder.toStringEncoder[F, LocalDate])
  given encodeStringLocalTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalTime](DateTimeFormatter.ISO_LOCAL_TIME))
  given encodeStringLocalDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME))
  given encodeStringMonthDay[F[_]: Applicative]: HighPriority[Encoder[F, String, MonthDay]] =
    HighPriority(Encoder.toStringEncoder[F, MonthDay])
  given encodeStringOffsetTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetTime](DateTimeFormatter.ISO_OFFSET_TIME))
  given encodeStringOffsetDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, OffsetDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetDateTime](DateTimeFormatter.ISO_OFFSET_DATE_TIME))
  given encodeStringYear[F[_]: Applicative]: HighPriority[Encoder[F, String, Year]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, Year](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .toFormatter()
    ))
  given encodeStringYearMonth[F[_]: Applicative]: HighPriority[Encoder[F, String, YearMonth]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, YearMonth](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
    ))
  given encodeStringZonedDateTime[F[_]: Applicative]: HighPriority[Encoder[F, String, ZonedDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, ZonedDateTime](DateTimeFormatter.ISO_ZONED_DATE_TIME))
  given encodeStringZoneOffset[F[_]: Applicative]: HighPriority[Encoder[F, String, ZoneOffset]] =
    HighPriority(Encoder.toStringEncoder[F, ZoneOffset])
  given encodeStringCurrency[F[_]: Applicative]: HighPriority[Encoder[F, String, Currency]] =
    HighPriority(_.getCurrencyCode.pure[F])
end EncoderStringInstances1
