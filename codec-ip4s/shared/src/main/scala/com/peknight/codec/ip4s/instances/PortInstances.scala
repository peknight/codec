package com.peknight.codec.ip4s.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.comcast.ip4s.Port
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}

trait PortInstances extends PortInstances1:
  given numberEncodePort[F[_]: Applicative]: Encoder[F, Number, Port] =
    Encoder.instance[F, Number, Port](port => Number.fromInt(port.value).pure[F])

  given encodeNumberPort[F[_]: Applicative, S: NumberType]: Encoder[F, S, Port] =
    Encoder.numberEncoder[F, S, Port]

  given decodePort[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], DecodingFailure, Port] =
    Decoder.numberDecoder[F, S, Port]
end PortInstances
