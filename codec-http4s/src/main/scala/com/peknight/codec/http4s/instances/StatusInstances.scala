package com.peknight.codec.http4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}
import org.http4s.Status

trait StatusInstances extends StatusInstances1:
  given numberEncodeStatus[F[_]: Applicative]: Encoder[F, Number, Status] =
    Encoder.map[F, Number, Status](status => Number.fromInt(status.code))
  given encodeStatusN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Status] =
    Encoder.encodeN[F, S, Status]
  given decodeStatusNS[F[_]: Applicative, S: {NumberType, StringType, Show}]: Decoder[F, Cursor[S], Status] =
    Decoder.decodeNS[F, S, Status]
end StatusInstances
object StatusInstances extends StatusInstances
