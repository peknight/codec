package com.peknight.codec.instances.time

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder, Encoder}

import scala.concurrent.duration.{Duration, FiniteDuration}

trait FiniteDurationInstances:
  def numberEncodeFiniteDurationOfSeconds[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toSeconds))

  def stringEncodeFiniteDurationOfSeconds[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toSeconds.toString)

  def encodeFiniteDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeFiniteDurationOfSeconds)

  def encodeFiniteDurationOfSecondsS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeFiniteDurationOfSeconds)

  def ofSeconds(seconds: BigDecimal): FiniteDuration =
    Duration.fromNanos((seconds * BigDecimal(1000_000_000L)).longValue)

  def numberDecodeFiniteDurationOfSeconds[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(ofSeconds))

  def stringDecodeFiniteDurationOfSeconds[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeFiniteDurationOfSeconds)

  def decodeFiniteDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeFiniteDurationOfSeconds)

  def decodeFiniteDurationOfSecondsS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeFiniteDurationOfSeconds)

  def decodeFiniteDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeFiniteDurationOfSeconds)

  def numberCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeFiniteDurationOfSeconds,
      numberDecodeFiniteDurationOfSeconds)

  def stringCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeFiniteDurationOfSeconds, 
      stringDecodeFiniteDurationOfSeconds)

  def codecFiniteDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfSecondsN, decodeFiniteDurationOfSecondsN)

  def codecFiniteDurationOfSecondsS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfSecondsS, decodeFiniteDurationOfSecondsS)

  def codecFiniteDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfSecondsN, decodeFiniteDurationOfSecondsNS)

  def numberEncodeFiniteDurationOfMillis[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toMillis))

  def stringEncodeFiniteDurationOfMillis[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toMillis.toString)

  def encodeFiniteDurationOfMillisN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeFiniteDurationOfMillis)

  def encodeFiniteDurationOfMillisS[F[_]: Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeFiniteDurationOfMillis)

  def numberDecodeFiniteDurationOfMillis[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(epochMillis =>
      ofSeconds(epochMillis / BigDecimal(1000))
    ))

  def stringDecodeFiniteDurationOfMillis[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeFiniteDurationOfMillis)

  def decodeFiniteDurationOfMillisN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeFiniteDurationOfMillis)

  def decodeFiniteDurationOfMillisS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeFiniteDurationOfMillis)

  def decodeFiniteDurationOfMillisNS[F[_]: Applicative, S: {NumberType, StringType}]
  : Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeFiniteDurationOfMillis)

  def numberCodecDurationOfMillis[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeFiniteDurationOfMillis, 
      numberDecodeFiniteDurationOfMillis)

  def stringCodecDurationOfMillis[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeFiniteDurationOfMillis, 
      stringDecodeFiniteDurationOfMillis)

  def codecFiniteDurationOfMillisN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMillisN, decodeFiniteDurationOfMillisN)

  def codecFiniteDurationOfMillisS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMillisS, decodeFiniteDurationOfMillisS)

  def codecFiniteDurationOfMillisNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMillisN, decodeFiniteDurationOfMillisNS)

  def numberEncodeFiniteDurationOfMinutes[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toMinutes))

  def stringEncodeFiniteDurationOfMinutes[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toMinutes.toString)

  def encodeFiniteDurationOfMinutesN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeFiniteDurationOfMinutes)

  def encodeFiniteDurationOfMinutesS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeFiniteDurationOfMinutes)

  def numberDecodeFiniteDurationOfMinutes[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(minutes =>
      ofSeconds(minutes * BigDecimal(60))
    ))

  def stringDecodeFiniteDurationOfMinutes[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeFiniteDurationOfMinutes)

  def decodeFiniteDurationOfMinutesN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeFiniteDurationOfMinutes)

  def decodeFiniteDurationOfMinutesS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeFiniteDurationOfMinutes)

  def decodeFiniteDurationOfMinutesNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeFiniteDurationOfMinutes)

  def numberCodecDurationOfMinutes[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeFiniteDurationOfMinutes, 
      numberDecodeFiniteDurationOfMinutes)

  def stringCodecDurationOfMinutes[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeFiniteDurationOfMinutes, 
      stringDecodeFiniteDurationOfMinutes)

  def codecFiniteDurationOfMinutesN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMinutesN, decodeFiniteDurationOfMinutesN)

  def codecFiniteDurationOfMinutesS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMinutesS, decodeFiniteDurationOfMinutesS)

  def codecFiniteDurationOfMinutesNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfMinutesN, decodeFiniteDurationOfMinutesNS)

  def numberEncodeFiniteDurationOfDays[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toDays))

  def stringEncodeFiniteDurationOfDays[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toDays.toString)

  def encodeFiniteDurationOfDaysN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeFiniteDurationOfDays)

  def encodeFiniteDurationOfDaysS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeFiniteDurationOfDays)

  def numberDecodeFiniteDurationOfDays[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(days =>
      ofSeconds(days * BigDecimal(24L * 60 * 60))
    ))

  def stringDecodeFiniteDurationOfDays[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeFiniteDurationOfDays)

  def decodeFiniteDurationOfDaysN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeFiniteDurationOfDays)

  def decodeFiniteDurationOfDaysS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeFiniteDurationOfDays)

  def decodeFiniteDurationOfDaysNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeFiniteDurationOfDays)

  def numberCodecDurationOfDays[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeFiniteDurationOfDays, numberDecodeFiniteDurationOfDays)

  def stringCodecDurationOfDays[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeFiniteDurationOfDays, stringDecodeFiniteDurationOfDays)

  def codecFiniteDurationOfDaysN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfDaysN, decodeFiniteDurationOfDaysN)

  def codecFiniteDurationOfDaysS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfDaysS, decodeFiniteDurationOfDaysS)

  def codecFiniteDurationOfDaysNS[F[_] : Applicative, S: {NumberType, StringType}]
  : Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeFiniteDurationOfDaysN, decodeFiniteDurationOfDaysNS)
end FiniteDurationInstances
object FiniteDurationInstances extends FiniteDurationInstances
