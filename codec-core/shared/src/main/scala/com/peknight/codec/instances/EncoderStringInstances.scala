package com.peknight.codec.instances

import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.{Applicative, Functor}
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.{HighPriority, LowPriority}

import java.net.URI
import java.time.*
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField
import java.util.{Currency, UUID}
import scala.concurrent.duration.Duration

trait EncoderStringInstances:
  given encodeBoolean[F[_]: Applicative]: Encoder[F, String, Boolean] = Encoder.stringEncoder[F, Boolean]
  given encodeChar[F[_]: Applicative]: Encoder[F, String, Char] = Encoder.stringEncoder[F, Char]
  given encodeFloat[F[_]: Applicative]: Encoder[F, String, Float] = Encoder.stringEncoder[F, Float]
  given encodeDouble[F[_]: Applicative]: Encoder[F, String, Double] = Encoder.stringEncoder[F, Double]
  given encodeByte[F[_]: Applicative]: Encoder[F, String, Byte] = Encoder.stringEncoder[F, Byte]
  given encodeShort[F[_]: Applicative]: Encoder[F, String, Short] = Encoder.stringEncoder[F, Short]
  given encodeInt[F[_]: Applicative]: Encoder[F, String, Int] = Encoder.stringEncoder[F, Int]
  given encodeLong[F[_]: Applicative]: Encoder[F, String, Long] = Encoder.stringEncoder[F, Long]
  given encodeBigInt[F[_]: Applicative]: Encoder[F, String, BigInt] = Encoder.stringEncoder[F, BigInt]
  given encodeBigDecimal[F[_]: Applicative]: Encoder[F, String, BigDecimal] = Encoder.stringEncoder[F, BigDecimal]
  given encodeUUID[F[_]: Applicative]: Encoder[F, String, UUID] = Encoder.stringEncoder[F, UUID]
  given encodeURI[F[_]: Applicative]: Encoder[F, String, URI] = Encoder.stringEncoder[F, URI]
  given encodeDuration[F[_]: Applicative]: Encoder[F, String, Duration] = Encoder.stringEncoder[F, Duration]
  given encodeInstant[F[_]: Applicative]: Encoder[F, String, Instant] = Encoder.stringEncoder[F, Instant]
  given encodePeriod[F[_]: Applicative]: Encoder[F, String, Period] = Encoder.stringEncoder[F, Period]
  given encodeZoneId[F[_]: Applicative]: Encoder[F, String, ZoneId] with
    def encode(a: ZoneId): F[String] = a.getId.pure[F]
  end encodeZoneId
  given encodeLocalDate[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalDate]] =
    HighPriority(Encoder.stringEncoder[F, LocalDate])
  given encodeLocalTime[F[_]: Applicative]: HighPriority[Encoder[F, String, LocalTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalTime](DateTimeFormatter.ISO_LOCAL_TIME))
  given encodeLocalDateTime[F[_] : Applicative]: HighPriority[Encoder[F, String, LocalDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME))
  given encodeMonthDay[F[_] : Applicative]: HighPriority[Encoder[F, String, MonthDay]] =
    HighPriority(Encoder.stringEncoder[F, MonthDay])
  given encodeOffsetTime[F[_] : Applicative]: HighPriority[Encoder[F, String, OffsetTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetTime](DateTimeFormatter.ISO_OFFSET_TIME))
  given encodeOffsetDateTime[F[_] : Applicative]: HighPriority[Encoder[F, String, OffsetDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, OffsetDateTime](DateTimeFormatter.ISO_OFFSET_DATE_TIME))
  given encodeYear[F[_] : Applicative]: HighPriority[Encoder[F, String, Year]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, Year](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .toFormatter()
    ))
  given encodeYearMonth[F[_] : Applicative]: HighPriority[Encoder[F, String, YearMonth]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, YearMonth](DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
    ))
  given encodeZonedDateTime[F[_] : Applicative]: HighPriority[Encoder[F, String, ZonedDateTime]] =
    HighPriority(Encoder.stringEncodeJavaTime[F, ZonedDateTime](DateTimeFormatter.ISO_ZONED_DATE_TIME))
  given encodeZoneOffset[F[_] : Applicative]: HighPriority[Encoder[F, String, ZoneOffset]] =
    HighPriority(Encoder.stringEncoder[F, ZoneOffset])
  given encodeCurrency[F[_] : Applicative]: HighPriority[Encoder[F, String, Currency]] =
    HighPriority((a: Currency) => a.getCurrencyCode.pure[F])

  given stringEncoder[F[_], S, A](using functor: Functor[F], encoder: Encoder[F, String, A], stringType: StringType[S])
  : LowPriority[Encoder[F, S, A]] =
    LowPriority((a: A) => encoder.encode(a).map(str => stringType.to(str)))
  end stringEncoder
end EncoderStringInstances
