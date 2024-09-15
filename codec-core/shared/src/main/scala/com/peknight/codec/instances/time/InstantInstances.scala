package com.peknight.codec.instances.time

import cats.Applicative
import com.peknight.codec.Encoder
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}

import java.time.Instant

trait InstantInstances:
  def stringEncodeInstantToEpochSecond[F[_]: Applicative]: Encoder[F, String, Instant] =
    Encoder.map[F, String, Instant](_.getEpochSecond.toString)

  def stringEncodeInstantToEpochMilli[F[_]: Applicative]: Encoder[F, String, Instant] =
    Encoder.map[F, String, Instant](_.toEpochMilli.toString)

  def numberEncodeInstantToEpochSecond[F[_] : Applicative]: Encoder[F, Number, Instant] =
    Encoder.map[F, Number, Instant](instant => Number.fromLong(instant.getEpochSecond))

  def numberEncodeInstantToEpochMilli[F[_] : Applicative]: Encoder[F, Number, Instant] =
    Encoder.map[F, Number, Instant](instant => Number.fromLong(instant.toEpochMilli))

  def encodeInstantToEpochSecondS[F[_]: Applicative, S: StringType]: Encoder[F, S, Instant] = 
    Encoder.encodeS[F, S, Instant](using stringEncodeInstantToEpochSecond)

  def numberEncodeLong[F[_]: Applicative]: Encoder[F, Number, Long] =
    Encoder.map[F, Number, Long](Number.fromLong)

  def encodeLongN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Long] = Encoder.encodeN[F, S, Long]
end InstantInstances
object InstantInstances extends InstantInstances
