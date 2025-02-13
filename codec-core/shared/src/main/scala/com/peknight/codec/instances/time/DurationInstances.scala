package com.peknight.codec.instances.time

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder, Encoder}

import java.time.Duration
import scala.math.BigDecimal.RoundingMode

trait DurationInstances:
  def numberEncodeDurationOfSeconds[F[_] : Applicative]: Encoder[F, Number, Duration] =
    Encoder.map[F, Number, Duration](duration => Number.fromLong(duration.getSeconds))

  def stringEncodeDurationOfSeconds[F[_] : Applicative]: Encoder[F, String, Duration] =
    Encoder.map[F, String, Duration](_.getSeconds.toString)

  def encodeDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Duration] =
    Encoder.encodeN[F, S, Duration](using numberEncodeDurationOfSeconds)

  def encodeDurationOfSecondsS[F[_] : Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.encodeS[F, S, Duration](using stringEncodeDurationOfSeconds)

  def ofSeconds(seconds: BigDecimal): Duration =
    val sec = seconds.setScale(0, RoundingMode.FLOOR)
    Duration.ofSeconds(sec.longValue, ((seconds - sec) * BigDecimal(1000_000_000L)).longValue)

  def numberDecodeDurationOfSeconds[F[_] : Applicative]: Decoder[F, Number, Duration] =
    Decoder.numberDecodeNumberOption[F, Duration](_.toBigDecimal.map(ofSeconds))

  def stringDecodeDurationOfSeconds[F[_] : Applicative]: Decoder[F, String, Duration] =
    Decoder.stringDecodeWithNumberDecoder[F, Duration](using numberDecodeDurationOfSeconds)

  def decodeDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeN[F, S, Duration](using numberDecodeDurationOfSeconds)

  def decodeDurationOfSecondsS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeS[F, S, Duration](using stringDecodeDurationOfSeconds)

  def decodeDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeNS[F, S, Duration](using numberDecodeDurationOfSeconds)

  def numberCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, Number, Number, Duration] =
    Codec[F, Number, Number, Duration](using numberEncodeDurationOfSeconds, numberDecodeDurationOfSeconds)

  def stringCodecDurationOfSeconds[F[_] : Applicative]: Codec[F, String, String, Duration] =
    Codec[F, String, String, Duration](using stringEncodeDurationOfSeconds, stringDecodeDurationOfSeconds)

  def codecDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfSecondsN, decodeDurationOfSecondsN)

  def codecDurationOfSecondsS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfSecondsS, decodeDurationOfSecondsS)

  def codecDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfSecondsN, decodeDurationOfSecondsNS)

  def numberEncodeDurationOfMillis[F[_] : Applicative]: Encoder[F, Number, Duration] =
    Encoder.map[F, Number, Duration](duration => Number.fromLong(duration.toMillis))

  def stringEncodeDurationOfMillis[F[_] : Applicative]: Encoder[F, String, Duration] =
    Encoder.map[F, String, Duration](_.toMillis.toString)

  def encodeDurationOfMillisN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Duration] =
    Encoder.encodeN[F, S, Duration](using numberEncodeDurationOfMillis)

  def encodeDurationOfMillisS[F[_]: Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.encodeS[F, S, Duration](using stringEncodeDurationOfMillis)

  def numberDecodeDurationOfMillis[F[_] : Applicative]: Decoder[F, Number, Duration] =
    Decoder.numberDecodeNumberOption[F, Duration](_.toBigDecimal.map(epochMillis =>
      ofSeconds(epochMillis / BigDecimal(1000))
    ))

  def stringDecodeDurationOfMillis[F[_] : Applicative]: Decoder[F, String, Duration] =
    Decoder.stringDecodeWithNumberDecoder[F, Duration](using numberDecodeDurationOfMillis)

  def decodeDurationOfMillisN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeN[F, S, Duration](using numberDecodeDurationOfMillis)

  def decodeDurationOfMillisS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeS[F, S, Duration](using stringDecodeDurationOfMillis)

  def decodeDurationOfMillisNS[F[_]: Applicative, S: {NumberType, StringType}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeNS[F, S, Duration](using numberDecodeDurationOfMillis)

  def numberCodecDurationOfMillis[F[_] : Applicative]: Codec[F, Number, Number, Duration] =
    Codec[F, Number, Number, Duration](using numberEncodeDurationOfMillis, numberDecodeDurationOfMillis)

  def stringCodecDurationOfMillis[F[_] : Applicative]: Codec[F, String, String, Duration] =
    Codec[F, String, String, Duration](using stringEncodeDurationOfMillis, stringDecodeDurationOfMillis)

  def codecDurationOfMillisN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfMillisN, decodeDurationOfMillisN)

  def codecDurationOfMillisS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfMillisS, decodeDurationOfMillisS)

  def codecDurationOfMillisNS[F[_] : Applicative, S: {NumberType, StringType}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeDurationOfMillisN, decodeDurationOfMillisNS)
end DurationInstances
object DurationInstances extends DurationInstances
