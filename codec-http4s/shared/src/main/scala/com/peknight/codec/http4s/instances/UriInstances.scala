package com.peknight.codec.http4s.instances

import cats.{Applicative, Id}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.http4s.iso.decodingFailureIsomorphism0
import com.peknight.codec.sum.StringType
import org.http4s.Uri

trait UriInstances:
  given stringCodecUri[F[_]: Applicative]: Codec[F, String, String, Uri] =
    Codec.applicative[F, String, String, Uri](_.toString)(
      t => Uri.fromString(t).left.map(decodingFailureIsomorphism0[Id](None).from)
    )

  given codecUriS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], Uri] = Codec.codecS[F, S, Uri]
end UriInstances
