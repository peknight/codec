package com.peknight.codec.ip4s.instances

import cats.{Applicative, Show}
import com.comcast.ip4s.Port
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.number.Number
import com.peknight.codec.sum.{NumberType, StringType}
import com.peknight.codec.{Codec, Decoder}

trait PortInstances1:
  given numberDecodePort[F[_]: Applicative]: Decoder[F, Number, Port] =
    Decoder.numberDecodeNumberOption[F, Port](_.toInt.flatMap(Port.fromInt))
  given decodePortN[F[_]: Applicative, S: {NumberType, Show}]: Decoder[F, Cursor[S], Port] = Decoder.decodeN[F, S, Port]
  given stringCodecPort[F[_]: Applicative]: Codec[F, String, String, Port] = Codec.stringCodecWithNumberDecoder[F, Port]
  given codecPortS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Port] = Codec.codecS[F, S, Port]
end PortInstances1
