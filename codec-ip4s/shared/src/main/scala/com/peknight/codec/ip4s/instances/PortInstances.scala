package com.peknight.codec.ip4s.instances

import cats.Applicative
import com.comcast.ip4s.Port
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}

trait PortInstances extends PortInstances1:

  given numberEncodePort[F[_]: Applicative]: Encoder[F, Number, Port] =
    Encoder.map[F, Number, Port](port => Number.fromInt(port.value))

  given encodePortN[F[_]: Applicative, S: NumberType]: Encoder[F, S, Port] = Encoder.encodeN[F, S, Port]

  given decodePortNS[F[_]: Applicative, S: NumberType: StringType]: Decoder[F, Cursor[S], Port] =
    Decoder.decodeNS[F, S, Port]
end PortInstances
