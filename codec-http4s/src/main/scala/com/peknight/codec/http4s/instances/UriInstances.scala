package com.peknight.codec.http4s.instances

import cats.{Applicative, Id, Show}
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.http4s.iso.decodingFailureIsomorphism0
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Codec, Decoder, Encoder}
import org.http4s.Uri

trait UriInstances:
  given stringCodecUri[F[_]: Applicative]: Codec[F, String, String, Uri] =
    Codec.applicative[F, String, String, Uri](_.toString)(
      t => Uri.fromString(t).left.map(decodingFailureIsomorphism0[Id](None).from)
    )
  given encodeUriS[F[_]: Applicative, S: StringType]: Encoder[F, S, Uri] = Encoder.encodeS[F, S, Uri]
  given decodeUriS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Uri] = Decoder.decodeS[F, S, Uri]

  given stringCodecScheme[F[_]: Applicative]: Codec[F, String, String, Uri.Scheme] =
    Codec.applicative[F, String, String, Uri.Scheme](_.value)(
      t => Uri.Scheme.fromString(t).left.map(decodingFailureIsomorphism0[Id](None).from)
    )

  given encodeSchemeS[F[_]: Applicative, S: StringType]: Encoder[F, S, Uri.Scheme] = Encoder.encodeS[F, S, Uri.Scheme]
  given decodeSchemeS[F[_]: Applicative, S: {StringType, Show}]: Decoder[F, Cursor[S], Uri.Scheme] =
    Decoder.decodeS[F, S, Uri.Scheme]
end UriInstances
object UriInstances extends UriInstances
