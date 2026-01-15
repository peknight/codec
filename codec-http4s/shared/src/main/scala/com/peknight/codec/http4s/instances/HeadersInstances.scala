package com.peknight.codec.http4s.instances

import cats.{Applicative, Show}
import com.peknight.codec.ci.instances.ciString.given
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.{ObjectType, StringType}
import com.peknight.codec.{Decoder, Encoder}
import org.http4s.{Header, Headers}
import org.typelevel.ci.CIString

trait HeadersInstances:
  given objectEncodeHeaders[F[_]: Applicative, S: StringType]: Encoder[F, Object[String, S], Headers] =
    Encoder.objectEncodeMap[F, S, CIString, String]
      .contramap[Headers](_.headers.map(header => header.name -> header.value).toMap)

  given encodeHeadersO[F[_]: Applicative, S: {ObjectType, StringType}]: Encoder[F, S, Headers] =
    Encoder.encodeO[F, S, Headers]

  given decodeHeaders[F[_]: Applicative, S: {ObjectType, StringType, Show}]: Decoder[F, Cursor[S], Headers] =
    Decoder.decodeMapO[F, S, CIString, String].map(m => Headers(m.map(Header.Raw.apply).toList))
end HeadersInstances
object HeadersInstances extends HeadersInstances
