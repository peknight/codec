package com.peknight.codec.http4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Codec, Decoder, Encoder}
import org.http4s.Uri.Host

trait HostInstances:
  given stringCodecHost[F[_]: Applicative]: Codec[F, String, String, Host] =
    Codec.mapOption[F, String, String, Host](_.toString)(t => com.comcast.ip4s.Host.fromString(t).map(Host.fromIp4sHost))
  given encodeHostS[F[_]: Applicative, S: StringType]: Encoder[F, S, Host] = Encoder.encodeS[F, S, Host]
  given decodeHostS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Host] = Decoder.decodeS[F, S, Host]
end HostInstances
object HostInstances extends HostInstances
