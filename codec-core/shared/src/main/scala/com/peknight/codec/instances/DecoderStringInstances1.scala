package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{DecodingFailure, WrongClassTag}
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.{HighPriority, MidPriority}

import java.net.URI
import java.time.*
import java.util.{Currency, UUID}
import scala.reflect.ClassTag

trait DecoderStringInstances1:

  given decodeStringBoolean[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Boolean] =
    Decoder.instance[F, String, DecodingFailure, Boolean] { t =>
      Decoder.toBooleanOption(t) match
        case Some(b) => b.asRight.pure
        case None => WrongClassTag[Boolean].value(t).asLeft.pure
    }

  given decodeStringChar[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Char] =
    Decoder.instance[F, String, DecodingFailure, Char](t =>
      if t.length == 1 then t.head.asRight.pure else WrongClassTag[Char].value(t).asLeft.pure
    )
  given decodeStringFloat[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Float] =
    Decoder.decodeNumber[F, Float](_.toFloat)
  given decodeStringDouble[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Double] =
    Decoder.decodeNumber[F, Double](_.toDouble)
  given decodeStringByte[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Byte] =
    Decoder.decodeNumber[F, Byte](_.toByte)
  given decodeStringShort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Short] =
    Decoder.decodeNumber[F, Short](_.toShort)
  given decodeStringInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Int] =
    Decoder.decodeNumber[F, Int](_.toInt)
  given decodeStringLong[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Long] =
    Decoder.decodeNumber[F, Long](_.toLong)
  given decodeStringBigInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigInt] =
    Decoder.decodeNumber[F, BigInt](_.toBigInt)
  given decodeStringBigDecimal[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigDecimal] =
    Decoder.decodeNumber[F, BigDecimal](identity)
  given decodeStringUUID[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, UUID]] =
    HighPriority(Decoder.decodeWithTry[F, UUID](UUID.fromString))
  given decodeStringURI[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, URI]] =
    HighPriority(Decoder.decodeWithTry[F, URI](t => new URI(t)))
  given decodeStringDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Duration]] =
    HighPriority(Decoder.decodeWithTry[F, Duration](Duration.parse))
  given decodeStringInstant[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Instant]] =
    HighPriority(Decoder.decodeWithTry[F, Instant](Instant.parse))
  given decodeStringPeriod[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Period]] =
    HighPriority(Decoder.decodeWithTry[F, Period](Period.parse))
  given decodeStringZoneId[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneId]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneId](ZoneId.of))
  given decodeStringLocalDate[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDate]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDate](LocalDate.parse))
  given decodeStringLocalTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalTime](LocalTime.parse))
  given decodeStringLocalDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDateTime](LocalDateTime.parse))
  given decodeStringMonthDay[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, MonthDay]] =
    HighPriority(Decoder.decodeWithTry[F, MonthDay](MonthDay.parse))
  given decodeStringOffsetTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetTime](OffsetTime.parse))
  given decodeStringOffsetDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetDateTime](OffsetDateTime.parse))
  given decodeStringYear[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Year]] =
    HighPriority(Decoder.decodeWithTry[F, Year](Year.parse))
  given decodeStringYearMonth[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, YearMonth]] =
    HighPriority(Decoder.decodeWithTry[F, YearMonth](YearMonth.parse))
  given decodeStringZonedDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, ZonedDateTime](ZonedDateTime.parse))
  given decodeStringZoneOffset[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneOffset](ZoneOffset.of))
  given decodeStringCurrency[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Currency]] =
    HighPriority(Decoder.decodeWithTry[F, Currency](Currency.getInstance))
end DecoderStringInstances1
