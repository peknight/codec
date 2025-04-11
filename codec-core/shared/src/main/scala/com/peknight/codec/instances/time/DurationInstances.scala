package com.peknight.codec.instances.time

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder, Encoder}

import scala.concurrent.duration.{Duration, FiniteDuration}

trait DurationInstances:
  def numberEncodeDurationOfSeconds[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toSeconds))

  def stringEncodeDurationOfSeconds[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toSeconds.toString)

  def encodeDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeDurationOfSeconds)

  def encodeDurationOfSecondsS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeDurationOfSeconds)

  def ofSeconds(seconds: BigDecimal): FiniteDuration =
    Duration.fromNanos((seconds * BigDecimal(1000_000_000L)).longValue)

  def numberDecodeDurationOfSeconds[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(ofSeconds))

  def stringDecodeDurationOfSeconds[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeDurationOfSeconds)

  def decodeDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeDurationOfSeconds)

  def decodeDurationOfSecondsS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeDurationOfSeconds)

  def decodeDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeDurationOfSeconds)

  def numberCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeDurationOfSeconds, numberDecodeDurationOfSeconds)

  def stringCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeDurationOfSeconds, stringDecodeDurationOfSeconds)

  def codecDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfSecondsN, decodeDurationOfSecondsN)

  def codecDurationOfSecondsS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfSecondsS, decodeDurationOfSecondsS)

  def codecDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfSecondsN, decodeDurationOfSecondsNS)

  def numberEncodeDurationOfMillis[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toMillis))

  def stringEncodeDurationOfMillis[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toMillis.toString)

  def encodeDurationOfMillisN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeDurationOfMillis)

  def encodeDurationOfMillisS[F[_]: Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeDurationOfMillis)

  def numberDecodeDurationOfMillis[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(epochMillis =>
      ofSeconds(epochMillis / BigDecimal(1000))
    ))

  def stringDecodeDurationOfMillis[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeDurationOfMillis)

  def decodeDurationOfMillisN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeDurationOfMillis)

  def decodeDurationOfMillisS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeDurationOfMillis)

  def decodeDurationOfMillisNS[F[_]: Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeDurationOfMillis)

  def numberCodecDurationOfMillis[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeDurationOfMillis, numberDecodeDurationOfMillis)

  def stringCodecDurationOfMillis[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeDurationOfMillis, stringDecodeDurationOfMillis)

  def codecDurationOfMillisN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfMillisN, decodeDurationOfMillisN)

  def codecDurationOfMillisS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfMillisS, decodeDurationOfMillisS)

  def codecDurationOfMillisNS[F[_] : Applicative, S: {NumberType, StringType}]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfMillisN, decodeDurationOfMillisNS)

  def numberEncodeDurationOfDays[F[_] : Applicative]: Encoder[F, Number, FiniteDuration] =
    Encoder.map[F, Number, FiniteDuration](duration => Number.fromLong(duration.toDays))

  def stringEncodeDurationOfDays[F[_] : Applicative]: Encoder[F, String, FiniteDuration] =
    Encoder.map[F, String, FiniteDuration](_.toDays.toString)

  def encodeDurationOfDaysN[F[_] : Applicative, S: NumberType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeN[F, S, FiniteDuration](using numberEncodeDurationOfDays)

  def encodeDurationOfDaysS[F[_] : Applicative, S: StringType]: Encoder[F, S, FiniteDuration] =
    Encoder.encodeS[F, S, FiniteDuration](using stringEncodeDurationOfDays)

  def numberDecodeDurationOfDays[F[_] : Applicative]: Decoder[F, Number, FiniteDuration] =
    Decoder.numberDecodeNumberOption[F, FiniteDuration](_.toBigDecimal.map(days =>
      ofSeconds(days * BigDecimal(24L * 60 * 60))
    ))

  def stringDecodeDurationOfDays[F[_] : Applicative]: Decoder[F, String, FiniteDuration] =
    Decoder.stringDecodeWithNumberDecoder[F, FiniteDuration](using numberDecodeDurationOfDays)

  def decodeDurationOfDaysN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeN[F, S, FiniteDuration](using numberDecodeDurationOfDays)

  def decodeDurationOfDaysS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeS[F, S, FiniteDuration](using stringDecodeDurationOfDays)

  def decodeDurationOfDaysNS[F[_] : Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], FiniteDuration] =
    Decoder.decodeNS[F, S, FiniteDuration](using numberDecodeDurationOfDays)

  def numberCodecDurationOfDays[F[_] : Applicative]: Codec[F, Number, Number, FiniteDuration] =
    Codec[F, Number, Number, FiniteDuration](using numberEncodeDurationOfDays, numberDecodeDurationOfDays)

  def stringCodecDurationOfDays[F[_] : Applicative]: Codec[F, String, String, FiniteDuration] =
    Codec[F, String, String, FiniteDuration](using stringEncodeDurationOfDays, stringDecodeDurationOfDays)

  def codecDurationOfDaysN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfDaysN, decodeDurationOfDaysN)

  def codecDurationOfDaysS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfDaysS, decodeDurationOfDaysS)

  def codecDurationOfDaysNS[F[_] : Applicative, S: {NumberType, StringType}]: Codec[F, S, Cursor[S], FiniteDuration] =
    Codec[F, S, Cursor[S], FiniteDuration](using encodeDurationOfDaysN, decodeDurationOfDaysNS)

end DurationInstances
object DurationInstances extends DurationInstances
