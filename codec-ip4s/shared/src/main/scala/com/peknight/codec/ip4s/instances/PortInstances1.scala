package com.peknight.codec.ip4s.instances

import cats.Applicative
import com.comcast.ip4s.Port
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Decoder, Encoder}

trait PortInstances1:
  given stringEncodePort[F[_]: Applicative]: Encoder[F, String, Port] = Encoder.toStringEncoder[F, Port]

  given encodeStringPort[F[_]: Applicative, S: StringType]: Encoder[F, S, Port] = Encoder.stringEncoder[F, S, Port]

  given numberDecodePort[F[_]: Applicative]: Decoder[F, Number, DecodingFailure, Port] =
    Decoder.numberDecodeNumberOption[F, Port](_.toInt.flatMap(Port.fromInt))

  given decodeStrictPort[F[_]: Applicative, S: NumberType]: Decoder[F, Cursor[S], DecodingFailure, Port] =
    Decoder.strictNumberDecoder[F, S, Port]

  given stringDecodePort[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Port] =
    Decoder.stringDecodeNumberOption[F, Port](_.toInt.flatMap(Port.fromInt))

  given decodeStringPort[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Port] =
    Decoder.stringDecoder[F, S, Port]
end PortInstances1
