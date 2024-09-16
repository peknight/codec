package com.peknight.codec.instances.time

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.{Codec, Decoder, Encoder}
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}

import java.time.Instant

trait InstantInstances:
  def numberEncodeInstantOfEpochSecond[F[_] : Applicative]: Encoder[F, Number, Instant] =
    Encoder.map[F, Number, Instant](instant => Number.fromLong(instant.getEpochSecond))

  def stringEncodeInstantOfEpochSecond[F[_] : Applicative]: Encoder[F, String, Instant] =
    Encoder.map[F, String, Instant](_.getEpochSecond.toString)

  def encodeInstantOfEpochSecondN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Instant] =
    Encoder.encodeN[F, S, Instant](using numberEncodeInstantOfEpochSecond)

  def encodeInstantOfEpochSecondS[F[_] : Applicative, S: StringType]: Encoder[F, S, Instant] =
    Encoder.encodeS[F, S, Instant](using stringEncodeInstantOfEpochSecond)

  def numberDecodeInstantOfEpochSecond[F[_] : Applicative]: Decoder[F, Number, Instant] =
    Decoder.numberDecodeNumberOption[F, Instant](_.toLong.map(Instant.ofEpochSecond))

  def stringDecodeInstantOfEpochSecond[F[_] : Applicative]: Decoder[F, String, Instant] =
    Decoder.stringDecodeWithNumberDecoder[F, Instant](using numberDecodeInstantOfEpochSecond)

  def decodeInstantOfEpochSecondN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeN[F, S, Instant](using numberDecodeInstantOfEpochSecond)

  def decodeInstantOfEpochSecondS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeS[F, S, Instant](using stringDecodeInstantOfEpochSecond)

  def decodeInstantOfEpochSecondNS[F[_] : Applicative, S: NumberType : StringType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeNS[F, S, Instant](using numberDecodeInstantOfEpochSecond)

  def numberCodecInstantOfEpochSecond[F[_] : Applicative]: Codec[F, Number, Number, Instant] =
    Codec[F, Number, Number, Instant](using numberEncodeInstantOfEpochSecond, numberDecodeInstantOfEpochSecond)

  def stringCodecInstantOfEpochSecond[F[_] : Applicative]: Codec[F, String, String, Instant] =
    Codec[F, String, String, Instant](using stringEncodeInstantOfEpochSecond, stringDecodeInstantOfEpochSecond)

  def codecInstantOfEpochSecondN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochSecondN, decodeInstantOfEpochSecondN)

  def codecInstantOfEpochSecondS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochSecondS, decodeInstantOfEpochSecondS)

  def codecInstantOfEpochSecondNS[F[_] : Applicative, S: NumberType : StringType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochSecondN, decodeInstantOfEpochSecondNS)

  def numberEncodeInstantOfEpochMilli[F[_] : Applicative]: Encoder[F, Number, Instant] =
    Encoder.map[F, Number, Instant](instant => Number.fromLong(instant.toEpochMilli))

  def stringEncodeInstantOfEpochMilli[F[_] : Applicative]: Encoder[F, String, Instant] =
    Encoder.map[F, String, Instant](_.toEpochMilli.toString)

  def encodeInstantOfEpochMilliN[F[_] : Applicative, S: NumberType]: Encoder[F, S, Instant] =
    Encoder.encodeN[F, S, Instant](using numberEncodeInstantOfEpochMilli)

  def encodeInstantOfEpochMilliS[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] =
    Encoder.encodeS[F, S, Instant](using stringEncodeInstantOfEpochMilli)

  def numberDecodeInstantOfEpochMilli[F[_] : Applicative]: Decoder[F, Number, Instant] =
    Decoder.numberDecodeNumberOption[F, Instant](_.toLong.map(Instant.ofEpochMilli))

  def stringDecodeInstantOfEpochMilli[F[_] : Applicative]: Decoder[F, String, Instant] =
    Decoder.stringDecodeWithNumberDecoder[F, Instant](using numberDecodeInstantOfEpochMilli)

  def decodeInstantOfEpochMilliN[F[_] : Applicative, S: NumberType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeN[F, S, Instant](using numberDecodeInstantOfEpochMilli)

  def decodeInstantOfEpochMilliS[F[_] : Applicative, S: StringType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeS[F, S, Instant](using stringDecodeInstantOfEpochMilli)

  def decodeInstantOfEpochMilliNS[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], Instant] =
    Decoder.decodeNS[F, S, Instant](using numberDecodeInstantOfEpochMilli)

  def numberCodecInstantOfEpochMilli[F[_] : Applicative]: Codec[F, Number, Number, Instant] =
    Codec[F, Number, Number, Instant](using numberEncodeInstantOfEpochMilli, numberDecodeInstantOfEpochMilli)

  def stringCodecInstantOfEpochMilli[F[_] : Applicative]: Codec[F, String, String, Instant] =
    Codec[F, String, String, Instant](using stringEncodeInstantOfEpochMilli, stringDecodeInstantOfEpochMilli)

  def codecInstantOfEpochMilliN[F[_] : Applicative, S: NumberType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochMilliN, decodeInstantOfEpochMilliN)

  def codecInstantOfEpochMilliS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochMilliS, decodeInstantOfEpochMilliS)

  def codecInstantOfEpochMilliNS[F[_] : Applicative, S: NumberType : StringType]: Codec[F, S, Cursor[S], Instant] =
    Codec[F, S, Cursor[S], Instant](using encodeInstantOfEpochMilliN, decodeInstantOfEpochMilliNS)
end InstantInstances
object InstantInstances extends InstantInstances
