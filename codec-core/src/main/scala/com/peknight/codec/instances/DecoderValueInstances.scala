package com.peknight.codec.instances

import cats.syntax.either.*
import cats.{Applicative, Show}
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{NotNumber, WrongClassTag}
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.{Duration as JDuration, *}
import java.util.{Currency, Locale, UUID}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag

trait DecoderValueInstances extends DecoderValueInstances1:

  given decodeNumberN[F[_]: Applicative, S: {NumberType, Show}]: Decoder[F, Cursor[S], Number] =
    Decoder.cursorValueApplicative[F, S, Number] { t =>
      NumberType[S].asNumber(t) match
        case Some(n) => n.asRight
        case None => NotNumber.asLeft
    }

  given stringDecodeString[F[_]: Applicative]: Decoder[F, String, String] =
    Decoder.identity[F, String]

  given decodeStringS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], String] =
    Decoder.decodeS[F, S, String]

  given decodeBooleanBS[F[_] : Applicative, S: {BooleanType, StringType, Show}]: Decoder[F, Cursor[S], Boolean] =
    Decoder.decodeBS[F, S]

  given stringDecodeChar[F[_]: Applicative]: Decoder[F, String, Char] =
    Decoder.applicative[F, String, Char](t =>
      if t.length == 1 then t.head.asRight else WrongClassTag[Char].value(t).asLeft
    )

  given decodeCharS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Char] =
    Decoder.decodeS[F, S, Char]

  given decodeFloatNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Float] =
    Decoder.decodeNS[F, S, Float]

  given decodeDoubleNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Double] =
    Decoder.decodeNS[F, S, Double]
    
  given decodeByteNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Byte] =
    Decoder.decodeNS[F, S, Byte]
    
  given decodeShortNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Short] =
    Decoder.decodeNS[F, S, Short]

  given decodeIntNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Int] =
    Decoder.decodeNS[F, S, Int]

  given decodeLongNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Long] =
    Decoder.decodeNS[F, S, Long]

  given decodeBigIntNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], BigInt] =
    Decoder.decodeNS[F, S, BigInt]

  given decodeBigDecimalNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], BigDecimal] =
    Decoder.decodeNS[F, S, BigDecimal]
    
  given stringDecodeUUID[F[_]: Applicative]: HighPriority[Decoder[F, String, UUID]] =
    HighPriority(Decoder.parse[F, String, UUID](UUID.fromString))

  given decodeUUIDS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], UUID]] =
    HighPriority(Decoder.decodeS[F, S, UUID])

  given stringDecodeURI[F[_]: Applicative]: HighPriority[Decoder[F, String, URI]] =
    HighPriority(Decoder.parse[F, String, URI](URI(_)))

  given decodeURIS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], URI]] =
    HighPriority(Decoder.decodeS[F, S, URI])

  given stringDecodeJavaDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, JDuration]] =
    HighPriority(Decoder.parse[F, String, JDuration](JDuration.parse))

  given decodeJavaDurationS[F[_]: Applicative, S: {StringType, Show}]
  : HighPriority[Decoder[F, Cursor[S], JDuration]] =
    HighPriority(Decoder.decodeS[F, S, JDuration])

  given stringDecodeDuration[F[_] : Applicative]: HighPriority[Decoder[F, String, Duration]] =
    HighPriority(Decoder.parse[F, String, Duration](Duration.apply))

  given decodeDurationS[F[_] : Applicative, S: {StringType, Show}]
  : HighPriority[Decoder[F, Cursor[S], Duration]] =
    HighPriority(Decoder.decodeS[F, S, Duration])

  given stringDecodeFiniteDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, FiniteDuration]] =
    HighPriority(Decoder.parse[F, String, FiniteDuration] { s =>
      Duration(s) match
        case d: FiniteDuration => d
        case d => throw WrongClassTag[FiniteDuration].value(d)
    })

  given decodeFiniteDurationS[F[_] : Applicative, S: {StringType, Show}]
  : HighPriority[Decoder[F, Cursor[S], FiniteDuration]] =
    HighPriority(Decoder.decodeS[F, S, FiniteDuration])

  given stringDecodeInstant[F[_]: Applicative]: HighPriority[Decoder[F, String, Instant]] =
    HighPriority(Decoder.parse[F, String, Instant](Instant.parse))

  given decodeInstantS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], Instant]] =
    HighPriority(Decoder.decodeS[F, S, Instant])

  given stringDecodePeriod[F[_]: Applicative]: HighPriority[Decoder[F, String, Period]] =
    HighPriority(Decoder.parse[F, String, Period](Period.parse))

  given decodePeriodS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], Period]] =
    HighPriority(Decoder.decodeS[F, S, Period])

  given stringDecodeZoneId[F[_]: Applicative]: HighPriority[Decoder[F, String, ZoneId]] =
    HighPriority(Decoder.parse[F, String, ZoneId](ZoneId.of))

  given decodeZoneIdS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], ZoneId]] =
    HighPriority(Decoder.decodeS[F, S, ZoneId])

  given stringDecodeLocalDate[F[_]: Applicative]: HighPriority[Decoder[F, String, LocalDate]] =
    HighPriority(Decoder.parse[F, String, LocalDate](LocalDate.parse))

  given decodeLocalDateS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], LocalDate]] =
    HighPriority(Decoder.decodeS[F, S, LocalDate])

  given stringDecodeLocalTime[F[_]: Applicative]: HighPriority[Decoder[F, String, LocalTime]] =
    HighPriority(Decoder.parse[F, String, LocalTime](LocalTime.parse))

  given decodeLocalTimeS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], LocalTime]] =
    HighPriority(Decoder.decodeS[F, S, LocalTime])

  given stringDecodeLocalDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, LocalDateTime]] =
    HighPriority(Decoder.parse[F, String, LocalDateTime](LocalDateTime.parse))

  given decodeLocalDateTimeS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], LocalDateTime]] =
    HighPriority(Decoder.decodeS[F, S, LocalDateTime])

  given stringDecodeMonthDay[F[_]: Applicative]: HighPriority[Decoder[F, String, MonthDay]] =
    HighPriority(Decoder.parse[F, String, MonthDay](MonthDay.parse))

  given decodeMonthDayS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], MonthDay]] =
    HighPriority(Decoder.decodeS[F, S, MonthDay])

  given stringDecodeOffsetTime[F[_]: Applicative]: HighPriority[Decoder[F, String, OffsetTime]] =
    HighPriority(Decoder.parse[F, String, OffsetTime](OffsetTime.parse))

  given decodeOffsetTimeS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], OffsetTime]] =
    HighPriority(Decoder.decodeS[F, S, OffsetTime])

  given stringDecodeOffsetDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, OffsetDateTime]] =
    HighPriority(Decoder.parse[F, String, OffsetDateTime](OffsetDateTime.parse))

  given decodeOffsetDateTimeS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], OffsetDateTime]] =
    HighPriority(Decoder.decodeS[F, S, OffsetDateTime])

  given stringDecodeYear[F[_]: Applicative]: HighPriority[Decoder[F, String, Year]] =
    HighPriority(Decoder.parse[F, String, Year](Year.parse))

  given decodeYearS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], Year]] =
    HighPriority(Decoder.decodeS[F, S, Year])

  given stringDecodeYearMonth[F[_]: Applicative]: HighPriority[Decoder[F, String, YearMonth]] =
    HighPriority(Decoder.parse[F, String, YearMonth](YearMonth.parse))

  given decodeYearMonthS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], YearMonth]] =
    HighPriority(Decoder.decodeS[F, S, YearMonth])

  given stringDecodeZonedDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, ZonedDateTime]] =
    HighPriority(Decoder.parse[F, String, ZonedDateTime](ZonedDateTime.parse))

  given decodeZonedDateTimeS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], ZonedDateTime]] =
    HighPriority(Decoder.decodeS[F, S, ZonedDateTime])

  given stringDecodeZoneOffset[F[_]: Applicative]: HighPriority[Decoder[F, String, ZoneOffset]] =
    HighPriority(Decoder.parse[F, String, ZoneOffset](ZoneOffset.of))

  given decodeZoneOffsetS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], ZoneOffset]] =
    HighPriority(Decoder.decodeS[F, S, ZoneOffset])

  given stringDecodeCurrency[F[_]: Applicative]: HighPriority[Decoder[F, String, Currency]] =
    HighPriority(Decoder.parse[F, String, Currency](Currency.getInstance))

  given decodeCurrencyS[F[_]: Applicative, S: {StringType, Show}]: HighPriority[Decoder[F, Cursor[S], Currency]] =
    HighPriority(Decoder.decodeS[F, S, Currency])
end DecoderValueInstances
