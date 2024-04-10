package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{DecodingFailure, NotNumber, WrongClassTag}
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{BooleanType, NumberType, StringType}
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.util.{Currency, UUID}
import scala.reflect.ClassTag

trait DecoderValueInstances extends DecoderValueInstances1:
  given decodeNumber[F[_], S](using applicative: Applicative[F], numberType: NumberType[S])
  : Decoder[F, Cursor[S], DecodingFailure, Number] =
    Decoder.cursor[F, S, Number] { t =>
      numberType.asNumber(t.value) match
        case Some(n) => n.asRight.pure
        case None => NotNumber.cursor(t).asLeft.pure
    }

  given stringDecodeString[F[_]: Applicative]: Decoder[F, String, DecodingFailure, String] =
    Decoder.instance[F, String, DecodingFailure, String](_.asRight.pure)

  given decodeString[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, String] =
    Decoder.stringDecoder[F, S, String]

  given decodeBoolean[F[_] : Applicative, S: BooleanType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.booleanDecoder[F, S]

  given stringDecodeChar[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Char] =
    Decoder.instance[F, String, DecodingFailure, Char](t =>
      if t.length == 1 then t.head.asRight.pure else WrongClassTag[Char].value(t).asLeft.pure
    )

  given decodeChar[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Char] =
    Decoder.stringDecoder[F, S, Char]

  given decodeFloat[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.numberDecoder[F, S, Float]

  given decodeDouble[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.numberDecoder[F, S, Double]
    
  given decodeByte[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.numberDecoder[F, S, Byte]
    
  given decodeShort[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.numberDecoder[F, S, Short]

  given decodeInt[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.numberDecoder[F, S, Int]

  given decodeLong[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.numberDecoder[F, S, Long]

  given decodeBigInt[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.numberDecoder[F, S, BigInt]

  given decodeBigDecimal[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.numberDecoder[F, S, BigDecimal]
    
  given stringDecodeUUID[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, UUID]] =
    HighPriority(Decoder.decodeWithTry[F, UUID](UUID.fromString))

  given decodeUUID[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, UUID]] =
    HighPriority(Decoder.stringDecoder[F, S, UUID])

  given stringDecodeURI[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, URI]] =
    HighPriority(Decoder.decodeWithTry[F, URI](t => new URI(t)))

  given decodeURI[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, URI]] =
    HighPriority(Decoder.stringDecoder[F, S, URI])

  given stringDecodeDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Duration]] =
    HighPriority(Decoder.decodeWithTry[F, Duration](Duration.parse))

  given decodeDuration[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, Duration]] =
    HighPriority(Decoder.stringDecoder[F, S, Duration])

  given stringDecodeInstant[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Instant]] =
    HighPriority(Decoder.decodeWithTry[F, Instant](Instant.parse))

  given decodeInstant[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Instant]] =
    HighPriority(Decoder.stringDecoder[F, S, Instant])

  given stringDecodePeriod[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Period]] =
    HighPriority(Decoder.decodeWithTry[F, Period](Period.parse))

  given decodePeriod[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Period]] =
    HighPriority(Decoder.stringDecoder[F, S, Period])

  given stringDecodeZoneId[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneId]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneId](ZoneId.of))

  given decodeZoneId[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneId]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneId])

  given stringDecodeLocalDate[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDate]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDate](LocalDate.parse))

  given decodeLocalDate[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDate]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDate])

  given stringDecodeLocalTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalTime](LocalTime.parse))

  given decodeLocalTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalTime])

  given stringDecodeLocalDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDateTime](LocalDateTime.parse))

  given decodeLocalDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDateTime])

  given stringDecodeMonthDay[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, MonthDay]] =
    HighPriority(Decoder.decodeWithTry[F, MonthDay](MonthDay.parse))

  given decodeMonthDay[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, MonthDay]] =
    HighPriority(Decoder.stringDecoder[F, S, MonthDay])

  given stringDecodeOffsetTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetTime](OffsetTime.parse))

  given decodeOffsetTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetTime])

  given stringDecodeOffsetDateTime[F[_]: Applicative]
  : HighPriority[Decoder[F, String, DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetDateTime](OffsetDateTime.parse))

  given decodeOffsetDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetDateTime])

  given stringDecodeYear[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Year]] =
    HighPriority(Decoder.decodeWithTry[F, Year](Year.parse))

  given decodeYear[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Year]] =
    HighPriority(Decoder.stringDecoder[F, S, Year])

  given stringDecodeYearMonth[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, YearMonth]] =
    HighPriority(Decoder.decodeWithTry[F, YearMonth](YearMonth.parse))

  given decodeYearMonth[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, YearMonth]] =
    HighPriority(Decoder.stringDecoder[F, S, YearMonth])

  given stringDecodeZonedDateTime[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, ZonedDateTime](ZonedDateTime.parse))

  given decodeZonedDateTime[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, ZonedDateTime])

  given stringDecodeZoneOffset[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneOffset](ZoneOffset.of))

  given decodeZoneOffset[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneOffset])

  given stringDecodeCurrency[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Currency]] =
    HighPriority(Decoder.decodeWithTry[F, Currency](Currency.getInstance))

  given decodeCurrency[F[_]: Applicative, S: StringType]
  : HighPriority[Decoder[F, Cursor[S], DecodingFailure, Currency]] =
    HighPriority(Decoder.stringDecoder[F, S, Currency])
end DecoderValueInstances
