package com.peknight.codec.ci.instances

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Codec, Decoder, Encoder}
import org.typelevel.ci.CIString

trait CIStringInstances:
  given stringCodecCIString[F[_]: Applicative]: Codec[F, String, String, CIString] =
    Codec.map[F, String, String, CIString](_.toString)(CIString.apply)

  given encodeCIStringS[F[_]: Applicative, S: StringType]: Encoder[F, S, CIString] =
    Encoder.encodeS[F, S, CIString]

  given decodeCIStringS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], CIString] =
    Decoder.decodeS[F, S, CIString]
end CIStringInstances
object CIStringInstances extends CIStringInstances
