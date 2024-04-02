package com.peknight.codec.ip4s.instances

import cats.Applicative
import com.comcast.ip4s.Hostname
import com.peknight.codec.{Decoder, Encoder}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType

trait HostInstances:
  given stringEncodeHostname[F[_]: Applicative]: Encoder[F, String, Hostname] =
    Encoder.toStringEncoder[F, Hostname]
  given encodeHostname[F[_]: Applicative, S: StringType]: Encoder[F, S, Hostname] =
    Encoder.stringEncoder[F, S, Hostname]
  given stringDecodeHostname[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Hostname] =
    Decoder.decodeWithOption[F, Hostname](Hostname.fromString)
  given decodeHostname[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Hostname] =
    Decoder.stringDecoder[F, S, Hostname]
end HostInstances
object HostInstances extends HostInstances
