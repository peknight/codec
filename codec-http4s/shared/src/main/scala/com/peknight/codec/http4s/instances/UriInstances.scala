package com.peknight.codec.http4s.instances

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Decoder, Encoder}
import org.http4s.Uri

trait UriInstances:
  given stringEncodeUri[F[_]: Applicative]: Encoder[F, String, Uri] = Encoder.toStringEncoder[F, Uri]
  given encodeUri[F[_]: Applicative, S: StringType]: Encoder[F, S, Uri] = Encoder.stringEncoder[F, S, Uri]

  given stringDecodeUri[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Uri] =
    Decoder.instance[F, String, DecodingFailure, Uri] { t =>
      Uri.fromString(t).left.map(DecodingFailure.apply).pure[F]
    }
  given decodeUri[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Uri] =
    Decoder.stringDecoder[F, S, Uri]
end UriInstances
