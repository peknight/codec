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
  given decodeString[F[_], S](using applicative: Applicative[F], stringType: StringType[S])
  : Decoder[F, Cursor[S], DecodingFailure, String] =
    Decoder.cursor[F, S, String] { t =>
      StringType[S].asString(t.value) match
        case Some(s) => s.asRight.pure
        case None => WrongClassTag[String].cursor(t).asLeft.pure
    }
  given decodeBoolean[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Boolean] =
    Decoder.stringDecoder[F, S, Boolean](decodeStringBoolean[F])
  given decodeChar[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Char] =
    Decoder.stringDecoder[F, S, Char](decodeStringChar[F])
  given decodeFloat[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Float] =
    Decoder.stringDecoder[F, S, Float](decodeStringFloat[F])
  given decodeDouble[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Double] =
    Decoder.stringDecoder[F, S, Double](decodeStringDouble[F])
  given decodeByte[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Byte] =
    Decoder.stringDecoder[F, S, Byte](decodeStringByte[F])
  given decodeShort[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Short] =
    Decoder.stringDecoder[F, S, Short](decodeStringShort[F])
  given decodeInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Int] =
    Decoder.stringDecoder[F, S, Int](decodeStringInt[F])
  given decodeLong[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Long] =
    Decoder.stringDecoder[F, S, Long](decodeStringLong[F])
  given decodeBigInt[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigInt] =
    Decoder.stringDecoder[F, S, BigInt](decodeStringBigInt[F])
  given decodeBigDecimal[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, BigDecimal] =
    Decoder.stringDecoder[F, S, BigDecimal](decodeStringBigDecimal[F])
  given decodeUUID[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, UUID]] =
    HighPriority(Decoder.stringDecoder[F, S, UUID](decodeStringUUID[F].instance))
  given decodeURI[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, URI]] =
    HighPriority(Decoder.stringDecoder[F, S, URI](decodeStringURI[F].instance))
  given decodeDuration[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Duration]] =
    HighPriority(Decoder.stringDecoder[F, S, Duration](decodeStringDuration[F].instance))
  given decodeInstant[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Instant]] =
    HighPriority(Decoder.stringDecoder[F, S, Instant](decodeStringInstant[F].instance))
  given decodePeriod[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Period]] =
    HighPriority(Decoder.stringDecoder[F, S, Period](decodeStringPeriod[F].instance))
  given decodeZoneId[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneId]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneId](decodeStringZoneId[F].instance))
  given decodeLocalDate[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDate]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDate](decodeStringLocalDate[F].instance))
  given decodeLocalTime[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalTime](decodeStringLocalTime[F].instance))
  given decodeLocalDateTime[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, LocalDateTime](decodeStringLocalDateTime[F].instance))
  given decodeMonthDay[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, MonthDay]] =
    HighPriority(Decoder.stringDecoder[F, S, MonthDay](decodeStringMonthDay[F].instance))
  given decodeOffsetTime[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetTime](decodeStringOffsetTime[F].instance))
  given decodeOffsetDateTime[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, OffsetDateTime](decodeStringOffsetDateTime[F].instance))
  given decodeYear[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Year]] =
    HighPriority(Decoder.stringDecoder[F, S, Year](decodeStringYear[F].instance))
  given decodeYearMonth[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, YearMonth]] =
    HighPriority(Decoder.stringDecoder[F, S, YearMonth](decodeStringYearMonth[F].instance))
  given decodeZonedDateTime[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.stringDecoder[F, S, ZonedDateTime](decodeStringZonedDateTime[F].instance))
  given decodeZoneOffset[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.stringDecoder[F, S, ZoneOffset](decodeStringZoneOffset[F].instance))
  given decodeCurrency[F[_]: Applicative, S: StringType]: HighPriority[Decoder[F, Cursor[S], DecodingFailure, Currency]] =
    HighPriority(Decoder.stringDecoder[F, S, Currency](decodeStringCurrency[F].instance))
end DecoderStringInstances
