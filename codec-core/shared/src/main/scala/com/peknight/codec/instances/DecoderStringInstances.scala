package com.peknight.codec.instances

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.peknight.codec.Decoder
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{DecodingFailure, WrongClassTag}
import com.peknight.codec.sum.StringType
import com.peknight.generic.priority.HighPriority

import java.net.URI
import java.time.*
import java.util.{Currency, UUID}
import scala.reflect.ClassTag

trait DecoderStringInstances:

  given decodeBoolean[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Boolean] =
    Decoder.instance[F, String, DecodingFailure, Boolean] { t =>
      Decoder.toBooleanOption(t) match
        case Some(b) => b.asRight.pure
        case None => WrongClassTag[Boolean].value(t).asLeft.pure
    }
  given decodeChar[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Char] =
    Decoder.instance[F, String, DecodingFailure, Char](t =>
      if t.length == 1 then t.head.asRight.pure else WrongClassTag[Char].value(t).asLeft.pure
    )
  given decodeFloat[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Float] =
    Decoder.decodeNumber[F, Float](_.toFloat)
  given decodeDouble[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Double] =
    Decoder.decodeNumber[F, Double](_.toDouble)
  given decodeByte[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Byte] =
    Decoder.decodeNumber[F, Byte](_.toByte)
  given decodeShort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Short] =
    Decoder.decodeNumber[F, Short](_.toShort)
  given decodeInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Int] =
    Decoder.decodeNumber[F, Int](_.toInt)
  given decodeLong[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Long] =
    Decoder.decodeNumber[F, Long](_.toLong)
  given decodeBigInt[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigInt] =
    Decoder.decodeNumber[F, BigInt](_.toBigInt)
  given decodeBigDecimal[F[_]: Applicative]: Decoder[F, String, DecodingFailure, BigDecimal] =
    Decoder.decodeNumber[F, BigDecimal](identity)
  given decodeUUID[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, UUID]] =
    HighPriority(Decoder.decodeWithTry[F, UUID](UUID.fromString))
  given decodeURI[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, URI]] =
    HighPriority(Decoder.decodeWithTry[F, URI](t => new URI(t)))
  given decodeDuration[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Duration]] =
    HighPriority(Decoder.decodeWithTry[F, Duration](Duration.parse))
  given decodeInstant[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Instant]] =
    HighPriority(Decoder.decodeWithTry[F, Instant](Instant.parse))
  given decodePeriod[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Period]] =
    HighPriority(Decoder.decodeWithTry[F, Period](Period.parse))
  given decodeZoneId[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneId]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneId](ZoneId.of))
  given decodeLocalDate[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDate]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDate](LocalDate.parse))
  given decodeLocalTime[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalTime](LocalTime.parse))
  given decodeLocalDateTime[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, LocalDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, LocalDateTime](LocalDateTime.parse))
  given decodeMonthDay[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, MonthDay]] =
    HighPriority(Decoder.decodeWithTry[F, MonthDay](MonthDay.parse))
  given decodeOffsetTime[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetTime](OffsetTime.parse))
  given decodeOffsetDateTime[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, OffsetDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, OffsetDateTime](OffsetDateTime.parse))
  given decodeYear[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Year]] =
    HighPriority(Decoder.decodeWithTry[F, Year](Year.parse))
  given decodeYearMonth[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, YearMonth]] =
    HighPriority(Decoder.decodeWithTry[F, YearMonth](YearMonth.parse))
  given decodeZonedDateTime[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZonedDateTime]] =
    HighPriority(Decoder.decodeWithTry[F, ZonedDateTime](ZonedDateTime.parse))
  given decodeZoneOffset[F[_] : Applicative]: HighPriority[Decoder[F, String, DecodingFailure, ZoneOffset]] =
    HighPriority(Decoder.decodeWithTry[F, ZoneOffset](ZoneOffset.of))
  given decodeCurrency[F[_]: Applicative]: HighPriority[Decoder[F, String, DecodingFailure, Currency]] =
    HighPriority(Decoder.decodeWithTry[F, Currency](Currency.getInstance))

  given stringDecoder[F[_], S, A](using applicative: Applicative[F], decoder: Decoder[F, String, DecodingFailure, A],
                                  stringType: StringType[S], classTag: ClassTag[A])
  : Decoder[F, Cursor[S], DecodingFailure, A] =
    Decoder.cursor[F, S, A] { t =>
      StringType[S].asString(t.value) match
        case Some(s) => decoder.decode(s).map(_.left.map(_.cursor(t)))
        case None => WrongClassTag[A].cursor(t).asLeft.pure
    }

end DecoderStringInstances
