package com.peknight.codec.instances.time

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder, Encoder}

import java.time.Duration
import scala.math.BigDecimal.RoundingMode

trait JavaDurationInstances:
  def numberEncodeJavaDurationOfSeconds[F[_] : Applicative]: Encoder[F, Number, Duration] =
    Encoder.map[F, Number, Duration](duration => Number.fromLong(duration.getSeconds))

  def stringEncodeJavaDurationOfSeconds[F[_] : Applicative]: Encoder[F, String, Duration] =
    Encoder.map[F, String, Duration](_.getSeconds.toString)

  def encodeJavaDurationOfSecondsN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Duration] =
    Encoder.encodeN[F, S, Duration](using numberEncodeJavaDurationOfSeconds)

  def encodeJavaDurationOfSecondsS[F[_] : Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.encodeS[F, S, Duration](using stringEncodeJavaDurationOfSeconds)

  def ofSeconds(seconds: BigDecimal): Duration =
    val sec = seconds.setScale(0, RoundingMode.FLOOR)
    Duration.ofSeconds(sec.longValue, ((seconds - sec) * BigDecimal(1000_000_000L)).longValue)

  def numberDecodeJavaDurationOfSeconds[F[_] : Applicative]: Decoder[F, Number, Duration] =
    Decoder.numberDecodeNumberOption[F, Duration](_.toBigDecimal.map(ofSeconds))

  def stringDecodeJavaDurationOfSeconds[F[_] : Applicative]: Decoder[F, String, Duration] =
    Decoder.stringDecodeWithNumberDecoder[F, Duration](using numberDecodeJavaDurationOfSeconds)

  def decodeJavaDurationOfSecondsN[F[_] : Applicative, S: {NumberType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeN[F, S, Duration](using numberDecodeJavaDurationOfSeconds)

  def decodeJavaDurationOfSecondsS[F[_] : Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeS[F, S, Duration](using stringDecodeJavaDurationOfSeconds)

  def decodeJavaDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeNS[F, S, Duration](using numberDecodeJavaDurationOfSeconds)

  def numberCodecJavaDurationOfSeconds[F[_] : Applicative]: Codec[F, Number, Number, Duration] =
    Codec[F, Number, Number, Duration](using numberEncodeJavaDurationOfSeconds, numberDecodeJavaDurationOfSeconds)

  def stringCodecJavaDurationOfSeconds[F[_] : Applicative]: Codec[F, String, String, Duration] =
    Codec[F, String, String, Duration](using stringEncodeJavaDurationOfSeconds, stringDecodeJavaDurationOfSeconds)

  def codecJavaDurationOfSecondsN[F[_] : Applicative, S: {NumberType, Show}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfSecondsN, decodeJavaDurationOfSecondsN)

  def codecJavaDurationOfSecondsS[F[_] : Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfSecondsS, decodeJavaDurationOfSecondsS)

  def codecJavaDurationOfSecondsNS[F[_] : Applicative, S: {NumberType, StringType, Show}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfSecondsN, decodeJavaDurationOfSecondsNS)

  def numberEncodeJavaDurationOfMillis[F[_] : Applicative]: Encoder[F, Number, Duration] =
    Encoder.map[F, Number, Duration](duration => Number.fromLong(duration.toMillis))

  def stringEncodeJavaDurationOfMillis[F[_] : Applicative]: Encoder[F, String, Duration] =
    Encoder.map[F, String, Duration](_.toMillis.toString)

  def encodeJavaDurationOfMillisN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Duration] =
    Encoder.encodeN[F, S, Duration](using numberEncodeJavaDurationOfMillis)

  def encodeJavaDurationOfMillisS[F[_]: Applicative, S: StringType]: Encoder[F, S, Duration] =
    Encoder.encodeS[F, S, Duration](using stringEncodeJavaDurationOfMillis)

  def numberDecodeJavaDurationOfMillis[F[_] : Applicative]: Decoder[F, Number, Duration] =
    Decoder.numberDecodeNumberOption[F, Duration](_.toBigDecimal.map(epochMillis =>
      ofSeconds(epochMillis / BigDecimal(1000))
    ))

  def stringDecodeJavaDurationOfMillis[F[_] : Applicative]: Decoder[F, String, Duration] =
    Decoder.stringDecodeWithNumberDecoder[F, Duration](using numberDecodeJavaDurationOfMillis)

  def decodeJavaDurationOfMillisN[F[_] : Applicative, S: {NumberType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeN[F, S, Duration](using numberDecodeJavaDurationOfMillis)

  def decodeJavaDurationOfMillisS[F[_] : Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeS[F, S, Duration](using stringDecodeJavaDurationOfMillis)

  def decodeJavaDurationOfMillisNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Duration] =
    Decoder.decodeNS[F, S, Duration](using numberDecodeJavaDurationOfMillis)

  def numberCodecJavaDurationOfMillis[F[_] : Applicative]: Codec[F, Number, Number, Duration] =
    Codec[F, Number, Number, Duration](using numberEncodeJavaDurationOfMillis, numberDecodeJavaDurationOfMillis)

  def stringCodecJavaDurationOfMillis[F[_] : Applicative]: Codec[F, String, String, Duration] =
    Codec[F, String, String, Duration](using stringEncodeJavaDurationOfMillis, stringDecodeJavaDurationOfMillis)

  def codecJavaDurationOfMillisN[F[_] : Applicative, S: {NumberType, Show}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfMillisN, decodeJavaDurationOfMillisN)

  def codecJavaDurationOfMillisS[F[_] : Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfMillisS, decodeJavaDurationOfMillisS)

  def codecJavaDurationOfMillisNS[F[_] : Applicative, S: {NumberType, StringType, Show}]
  : Codec[F, S, Cursor[S], Duration] =
    Codec[F, S, Cursor[S], Duration](using encodeJavaDurationOfMillisN, decodeJavaDurationOfMillisNS)
end JavaDurationInstances
object JavaDurationInstances extends JavaDurationInstances
