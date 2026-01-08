package com.peknight.codec.ip4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.ip4s.HostPort

trait HostPortInstances:
  given stringCodecHostPort[F[_]: Applicative]: Codec[F, String, String, HostPort] =
    Codec.mapOption[F, String, String, HostPort](_.toString)(HostPort.fromString)

  given codecHostPortS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], HostPort] =
    Codec.codecS[F, S, HostPort]
end HostPortInstances
object HostPortInstances extends HostPortInstances
