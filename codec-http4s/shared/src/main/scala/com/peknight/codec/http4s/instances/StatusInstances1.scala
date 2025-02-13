package com.peknight.codec.http4s.instances

import cats.Applicative
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.{DecodingFailure, WrongClassTag}
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}
import org.http4s.Status

trait StatusInstances1:
  given stringEncodeStatus[F[_]: Applicative]: Encoder[F, String, Status] =
    Encoder.map[F, String, Status](status => s"${status.code}")
  given encodeStatusS[F[_]: Applicative, S: StringType]: Encoder[F, S, Status] =
    Encoder.encodeS[F, S, Status]

  given numberDecodeStatus[F[_]: Applicative]: Decoder[F, Number, Status] =
    Decoder.applicative[F, Number, Status] { number =>
      number.toInt.toRight(WrongClassTag[Status].value(number))
        .flatMap(code => Status.fromInt(code))
        .left.map(DecodingFailure.apply)
    }

  given decodeStatusN[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], Status] =
    Decoder.decodeN[F, S, Status]

  given stringDecodeStatus[F[_]: Applicative]: Decoder[F, String, Status] =
    Decoder.stringDecodeWithNumberDecoder[F, Status]

  given decodeStatusS[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], Status] =
    Decoder.decodeS[F, S, Status]
end StatusInstances1
