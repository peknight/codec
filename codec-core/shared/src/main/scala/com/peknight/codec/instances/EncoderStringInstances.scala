package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.Encoder
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.util.{Currency, UUID}
import scala.concurrent.duration.Duration

trait EncoderStringInstances extends EncoderStringInstances1:
  given encodeString[F[_], S](using applicative: Applicative[F], stringType: StringType[S]): Encoder[F, S, String] with
    def encode(a: String): F[S] = stringType.to(a).pure[F]
  end encodeString
  given encodeBoolean[F[_]: Applicative, S: StringType]: Encoder[F, S, Boolean] =
    Encoder.stringEncoder[F, S, Boolean](encodeStringBoolean[F])
  given encodeChar[F[_]: Applicative, S: StringType]: Encoder[F, S, Char] =
    Encoder.stringEncoder[F, S, Char](encodeStringChar[F])
  given encodeFloat[F[_]: Applicative, S: StringType]: Encoder[F, S, Float] =
    Encoder.stringEncoder[F, S, Float](encodeStringFloat[F])
  given encodeDouble[F[_]: Applicative, S: StringType]: Encoder[F, S, Double] =
    Encoder.stringEncoder[F, S, Double](encodeStringDouble[F])
  given encodeByte[F[_]: Applicative, S: StringType]: Encoder[F, S, Byte] =
    Encoder.stringEncoder[F, S, Byte](encodeStringByte[F])
  given encodeShort[F[_]: Applicative, S: StringType]: Encoder[F, S, Short] =
    Encoder.stringEncoder[F, S, Short](encodeStringShort[F])
  given encodeInt[F[_]: Applicative, S: StringType]: Encoder[F, S, Int] =
    Encoder.stringEncoder[F, S, Int](encodeStringInt[F])
  given encodeLong[F[_]: Applicative, S: StringType]: Encoder[F, S, Long] =
    Encoder.stringEncoder[F, S, Long](encodeStringLong[F])
  given encodeBigInt[F[_]: Applicative, S: StringType]: Encoder[F, S, BigInt] =
    Encoder.stringEncoder[F, S, BigInt](encodeStringBigInt[F])
  given encodeBigDecimal[F[_]: Applicative, S: StringType]: Encoder[F, S, BigDecimal] =
    Encoder.stringEncoder[F, S, BigDecimal](encodeStringBigDecimal[F])
  given encodeUUID[F[_]: Applicative, S: StringType]: Encoder[F, S, UUID] =
    Encoder.stringEncoder[F, S, UUID](encodeStringUUID[F])
  given encodeURI[F[_]: Applicative, S: StringType]: Encoder[F, S, URI] =
    Encoder.stringEncoder[F, S, URI](encodeStringURI[F])
  given encodeDuration[F[_]: Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.stringEncoder[F, S, Duration](encodeStringDuration[F])
  given encodeInstant[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] =
    Encoder.stringEncoder[F, S, Instant](encodeStringInstant[F])
  given encodePeriod[F[_]: Applicative, S: StringType]: Encoder[F, S, Period] =
    Encoder.stringEncoder[F, S, Period](encodeStringPeriod[F])
  given encodeZoneId[F[_]: Applicative, S: StringType]: Encoder[F, S, ZoneId] =
    Encoder.stringEncoder[F, S, ZoneId](encodeStringZoneId[F])
  given encodeLocalDate[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDate]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDate](encodeStringLocalDate[F].instance))
  given encodeLocalTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalTime](encodeStringLocalTime[F].instance))
  given encodeLocalDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, LocalDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, LocalDateTime](encodeStringLocalDateTime[F].instance))
  given encodeMonthDay[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, MonthDay]] =
    HighPriority(Encoder.stringEncoder[F, S, MonthDay](encodeStringMonthDay[F].instance))
  given encodeOffsetTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetTime](encodeStringOffsetTime[F].instance))
  given encodeOffsetDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, OffsetDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, OffsetDateTime](encodeStringOffsetDateTime[F].instance))
  given encodeYear[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Year]] =
    HighPriority(Encoder.stringEncoder[F, S, Year](encodeStringYear[F].instance))
  given encodeYearMonth[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, YearMonth]] =
    HighPriority(Encoder.stringEncoder[F, S, YearMonth](encodeStringYearMonth[F].instance))
  given encodeZonedDateTime[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZonedDateTime]] =
    HighPriority(Encoder.stringEncoder[F, S, ZonedDateTime](encodeStringZonedDateTime[F].instance))
  given encodeZoneOffset[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, ZoneOffset]] =
    HighPriority(Encoder.stringEncoder[F, S, ZoneOffset](encodeStringZoneOffset[F].instance))
  given encodeCurrency[F[_]: Applicative, S: StringType]: HighPriority[Encoder[F, S, Currency]] =
    HighPriority(Encoder.stringEncoder[F, S, Currency](encodeStringCurrency[F].instance))
end EncoderStringInstances
